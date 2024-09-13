{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Debug.Pretty.Simple

import qualified Brick
import qualified Brick.BChan as Brick
import qualified Brick.Widgets.Border as Brick
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Platform.Unix as Vty
import qualified Options.Applicative as Opt

import Linear.V2
import Optics
import Optics.State.Operators
import Text.Read (readMaybe)
import Text.Printf

import Data.Bifunctor
import Data.Foldable
import Data.Maybe
import Data.Tuple
import Data.List
import Data.List.Split
import Data.Time

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Control.Concurrent
import Control.Exception
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import System.Random
import System.IO

data Marking
  = MarkNone
  | MarkFlag
  | MarkMark
  | MarkMine
  | MarkWrong
  deriving (Show, Eq, Ord, Enum, Bounded)

data Tile
  = TileOpen { count :: Int }
  | TileClosed { opened :: Maybe Int , mark :: Marking }
  deriving (Show, Eq, Ord)

data Options = Options
  { symbols :: Map String Char
  , colors  :: Map String Int
  , keys    :: Map (Vty.Key, [Vty.Modifier]) String
  , jump   :: Int
  , width  :: Int
  , height :: Int
  , mines  :: Int
  , autoChord :: Bool
  , autoFlag  :: Bool
  , mouseDrag :: Bool
  } deriving (Show)

data GameState
  = GameStart
  | GameRunning
  | GameFinished
  deriving (Show, Eq, Ord, Enum, Bounded)

data GameStats = GameStats
  { timeStart   :: UTCTime
  , timeNow     :: UTCTime
  , openTimes   :: Int
  , flagTimes   :: Int
  , moveTimes   :: Int
  , flagCount   :: Int
  , open3BV     :: Int
  , total3BV    :: Int
  , closedCount :: Int
  } deriving (Show)

defaultGameStats :: UTCTime -> GameStats
defaultGameStats time = GameStats
  { timeStart   = time
  , timeNow     = time
  , openTimes   = 0
  , flagTimes   = 0
  , moveTimes   = 0
  , flagCount   = 0
  , open3BV     = 0
  , total3BV    = 0
  , closedCount = 0
  }

type Field = Seq (Seq Tile)

data Game = Game
  { state   :: GameState
  , field   :: Field
  , options :: Options
  , cursor  :: V2 Int
  , mouse   :: Bool
  , stats   :: GameStats
  , random  :: StdGen
  } deriving (Show)

data Name
  = NameField
  | NameStats
  | NameCursor
  | NameTile (V2 Int)
  deriving (Show, Eq, Ord)

makePrisms ''Marking
makePrisms ''Tile
makePrisms ''GameState
makePrisms ''GameStats

makeFieldLabelsNoPrefix ''Marking
makeFieldLabelsNoPrefix ''Tile
makeFieldLabelsNoPrefix ''Options
makeFieldLabelsNoPrefix ''GameState
makeFieldLabelsNoPrefix ''GameStats
makeFieldLabelsNoPrefix ''Game

optionsParser :: Opt.Parser Options
optionsParser = do
  let dictParser :: Show a => String -> (a -> String) -> (String -> Maybe a)
        -> Opt.Mod Opt.OptionFields (String, a) -> [(String, a)]
        -> Opt.Parser (Map String a)
      dictParser name shower reader opts defaults = parser where
        defaults' = Map.fromList defaults
        merge x = Map.union defaults' (Map.fromList x)
        info = intercalate "\n" [k ++ "=" ++ shower v | (k, v) <- defaults]
        parser = merge . pTraceShowId <$> many (Opt.option readM (Opt.long name <> opts))
          -- <**> Opt.infoOption info (Opt.long ("list-" ++ name ++ "s")
               -- <> Opt.help ("list all avaiable " ++ name ++ "s"))
        readM = Opt.eitherReader $ \arg -> do
          (key, val) <- case break (== '=') arg of
            (k, '=':v) -> pure (k, v)
            _ -> throwError ""
          when (Map.notMember key defaults') $
            throwError $ "invalid " ++ name ++ ": " ++ show key
          val' <- case reader val of
            Nothing -> throwError
              $ "invalid " ++ name ++ " value: "
              ++ show key ++ "=" ++ val
            Just a -> pure a
          pure (key, val')
  symbols <- dictParser "symbol" pure
    (\case (x:_) -> Just x ; _ -> Nothing)
    (Opt.metavar "name=char" <> Opt.help "symbols for various tiles")
    [ ("zero",  '·'), ("one",    '1'), ("two",   '2')
    , ("three", '3'), ("four",   '4'), ("five",  '5')
    , ("six",   '6'), ("seven",  '7'), ("eight", '8')
    , ("flag",  '⚑'), ("mine",   '☢'), ("mark",  '?')
    , ("wrong", '✗'), ("closed", '·') ]
  colors <- dictParser "color" show readMaybe
    (Opt.metavar "name=int" <> Opt.help "colors for various tiles")
    [ ("zero",    8), ("one",    105), ("two",   120)
    , ("three", 210), ("four",    21), ("five",  196)
    , ("six",    33), ("seven",   15), ("eight",   7)
    , ("flag",    9), ("mine",     0), ("mark",   11)
    , ("wrong",  13), ("closed",   8), ("open",    0) ]
  let modifiers =
        [ ('M', Vty.MMeta) , ('S', Vty.MShift)
        , ('A', Vty.MAlt) , ('C', Vty.MCtrl) ]
      modifiers' = map swap modifiers
  let parseKeys :: String -> Maybe [(Vty.Key, [Vty.Modifier])]
      parseKeys xs = mapM parseKey (splitOn "," xs) where
        parseKey (span (== '-') -> (ms@(_:_), '-':k))
          = (,) <$> parseCode k <*> mapM parseMod ms
        parseKey k = (, []) <$> parseCode k
        parseCode ('F':n) = Vty.KFun <$> readMaybe n
        parseCode [c]     = Just (Vty.KChar c)
        parseCode x       = readMaybe ('K':x)
        parseMod = flip lookup modifiers
      showKeys :: [(Vty.Key, [Vty.Modifier])] -> String
      showKeys xs = intercalate "," keys where
        showKey (Vty.KFun n) = 'F' : show n
        showKey (Vty.KChar c) = [c]
        showKey k = tail (show k)
        showMod [] = ""
        showMod xs = mapMaybe (`lookup` modifiers') xs ++ "-"
        keys = [ showMod ms ++ showKey k | (k, ms) <- xs ]
      makeKeyMap :: Map String [(Vty.Key, [Vty.Modifier])]
        -> Map (Vty.Key, [Vty.Modifier]) String
      makeKeyMap keys = Map.fromList
        [ (k, n) | (n, ks) <- Map.toList keys , k <- ks ]
  keys <- makeKeyMap <$> dictParser "key" showKeys parseKeys
    (Opt.metavar "name=str" <> Opt.help "keys for various actions")
    [ ("up",         [(Vty.KChar 'u', []), (Vty.KUp,    [])])
    , ("down",       [(Vty.KChar 'j', []), (Vty.KDown,  [])])
    , ("left",       [(Vty.KChar 'h', []), (Vty.KLeft,  [])])
    , ("right",      [(Vty.KChar 'k', []), (Vty.KRight, [])])
    , ("open-up",    [(Vty.KChar 'U', []), (Vty.KUp,    [Vty.MShift])])
    , ("open-down",  [(Vty.KChar 'J', []), (Vty.KDown,  [Vty.MShift])])
    , ("open-left",  [(Vty.KChar 'H', []), (Vty.KLeft,  [Vty.MShift])])
    , ("open-right", [(Vty.KChar 'K', []), (Vty.KRight, [Vty.MShift])])
    , ("jump-up",    [(Vty.KUp,    [Vty.MMeta]), (Vty.KUp,    [Vty.MCtrl])])
    , ("jump-down",  [(Vty.KDown,  [Vty.MMeta]), (Vty.KDown,  [Vty.MCtrl])])
    , ("jump-left",  [(Vty.KLeft,  [Vty.MMeta]), (Vty.KLeft,  [Vty.MCtrl])])
    , ("jump-right", [(Vty.KRight, [Vty.MMeta]), (Vty.KRight, [Vty.MCtrl])])
    , ("flag",       [(Vty.KChar 'x', [])])
    , ("mark",       [(Vty.KChar 'c', [])])
    , ("open",       [(Vty.KChar 'z', []), (Vty.KChar ' ', [])])
    , ("restart",    [(Vty.KChar 'r', []), (Vty.KFun 2, [])])
    , ("quit",       [(Vty.KChar 'q', []), (Vty.KEsc, []), (Vty.KChar 'c', [Vty.MCtrl])]) ]
  jump <- Opt.option Opt.auto
    $ Opt.long "jump" <> Opt.metavar "int" <> Opt.value 5
    <> Opt.showDefault <> Opt.help "jump distance'"
  width <- Opt.option Opt.auto
    $ Opt.long "width" <> Opt.metavar "int" <> Opt.value 30
    <> Opt.showDefault <> Opt.help "'mine field width"
  height <- Opt.option Opt.auto
    $ Opt.long "height" <> Opt.metavar "int" <> Opt.value 16
    <> Opt.showDefault <> Opt.help "'mine field height"
  mines <- Opt.option Opt.auto
    $ Opt.long "mines" <> Opt.metavar "int" <> Opt.value 99
    <> Opt.showDefault <> Opt.help "'mine count"
  let binaryFlag name value opts =
        ( Opt.flag' (not value) (Opt.long (x ++ name) <> Opt.internal)
          <|> Opt.flag value (not value) (Opt.long (y ++ name) <> Opt.internal) )
        <**> Opt.infoOption "" (Opt.long ("[no]-" ++ name) <> opts)
        where (x, y) = if value then ("no-", "") else ("", "no-")
  autoChord <- binaryFlag "auto-chord" False
    (Opt.help "automatically chord on numbers")
  autoFlag <- binaryFlag "auto-flag" False
    (Opt.help "automatically flag neighbors on numbers")
  mouseDrag <- binaryFlag "mouse-drag" False
    (Opt.help "open/flag on drag rather than on click")
  pure Options{..}

emptyField :: Int -> Int -> Field
emptyField rows cols = Seq.replicate rows
  . Seq.replicate cols $ TileClosed (Just 0) MarkNone

sample :: RandomGen g => g -> Int -> [a] -> ([a], g)
sample g n xs = run g n (Seq.fromList xs) where
  run g 0 _ = ([], g)
  run g _ Seq.Empty = ([], g)
  run g n xs = (y : ys, g'') where
    (i, g') = randomR (0, Seq.length xs - 1) g
    (ls, y Seq.:<| rs) = Seq.splitAt i xs
    (ys, g'') = run g' (n - 1) (ls <> rs)

data GameException
  = OpenMineTile
  | AllTilesOpened
  deriving (Show, Eq)
instance Exception GameException

type GameM = ExceptT GameException (Brick.EventM Name Game)
runGame :: GameM () -> Brick.EventM Name Game ()
runGame m = runExceptT m >>= either handle pure where
  handle err = do
    Game{options=Options{..}, ..} <- get
    #state .= GameFinished
    ifor_ field $ \r -> itraverse_ $ \c -> \case
      TileClosed{opened=Just _, mark=MarkFlag} -> do
        #field % ix2 (V2 r c) % #mark .= MarkWrong
      TileClosed{opened=Nothing, ..} | mark /= MarkFlag -> do
        #field % ix2 (V2 r c) % #mark .= MarkMine
      _ -> pure ()

neighbors :: V2 Int -> [V2 Int]
neighbors n = map (+ n)
  [ V2 (-1) (-1), V2 (-1) 0, V2 (-1) 1
  , V2   0  (-1),            V2   0  1
  , V2   1  (-1), V2   1  0, V2   1  1 ]

around :: V2 Int -> Traversal' Field Tile
around n = foldl adjoin (adjoin ignored ignored) (map ix2 (neighbors n))

ix2 :: V2 Int -> AffineTraversal' Field Tile
ix2 (V2 r c) = ix r % ix c

field3BV :: Field -> Int
field3BV field = 0 where
  count :: Set (V2 Int) -> [V2 Int] -> Int
  count _ [] = 0
  count zs (x:xs) = case field ^? ix2 x of
    _ | x `Set.member` zs -> count zs xs
    Just TileClosed{opened=Just 0} ->
      1 + count (flood zs [x]) xs
    -- Just TileClosed{opened=Just _} ->
      -- 1 + count (flood zs [x]) xs
    _ -> count zs xs
  flood zs [] = zs
  flood zs (x:xs) = case field ^? ix2 x of
    _ | x `Set.member` zs -> flood zs xs
    Just TileClosed{opened=Just 0} ->
      flood zs' (neighbors x ++ xs)
    Just _ -> flood zs' xs
    Nothing -> flood zs xs
    where
    zs' = Set.insert x zs

gameGen :: V2 Int -> GameM ()
gameGen posn = do
  Game{options=Options{..}, ..} <- get
  #state .= GameRunning
  let (tiles, random') = sample random mines
        $ liftA2 V2 [0 .. height - 1] [0 .. width - 1]
        \\ (posn : neighbors posn)
  #random .= random'
  #field .= emptyField height width
  forM_ tiles $ \n -> do
    #field % ix2 n % #opened .= Nothing
    #field % around n % #opened % _Just %= (+ 1)
  field' <- use #field
  #stats % #flagCount .= mines
  #stats % #total3BV .= field3BV field'
  #stats % #closedCount .= lengthOf
    (traversed % traversed % #opened % _Just) field'
  time <- liftIO getCurrentTime
  #stats % #timeNow .= time
  #stats % #timeStart .= time
  gameOpen False posn

gameOpen :: Bool -> V2 Int -> GameM ()
gameOpen user posn = do
  Game{options=Options{..}, ..} <- get
  when (state == GameRunning) $
    when user $ #stats % #openTimes %= (+ 1)
  case state of
    GameStart -> gameGen posn
    GameRunning -> do
      case field ^? ix2 posn of
        Just TileOpen{} | user -> gameChord True posn
        Just TileClosed{} -> gameOpen' posn
        _ -> pure ()
    _ -> pure ()

gameOpen' :: V2 Int -> GameM ()
gameOpen' posn = do
  Game{options=Options{..}, ..} <- get
  case field ^? ix2 posn of
    Just TileClosed{mark=MarkFlag} -> pure ()
    Just TileClosed{opened=Nothing} -> throwError OpenMineTile
    Just TileClosed{opened=Just count} -> do
      #field % ix2 posn .= TileOpen count
      closed <- #stats % #closedCount <%= subtract 1
      when (closed == 0) $ throwError AllTilesOpened
      when (count == 0) . mapM_ gameOpen' $ neighbors posn
    _ -> pure ()

gameMove :: Bool -> V2 Int -> GameM ()
gameMove open move = do
  Game{options=Options{..}, ..} <- get
  when (state == GameRunning) $
    #stats % #moveTimes %= (+ 1)
  when open $ gameOpen False cursor
  let cursor' = Brick.clamp <$> 0
        <*> (V2 height width - 1)
        <*> (cursor + move)
  #cursor .= cursor'
  when open $ gameOpen False cursor'

gameFlag :: Bool -> V2 Int -> GameM ()
gameFlag user posn = do
  Game{options=Options{..}, ..} <- get
  when (state == GameRunning) $
    when user $ #stats % #flagTimes %= (+ 1)
  when (state == GameRunning) $ do
    case field ^? ix2 posn of
      Just TileOpen{} | user -> gameChord True posn
      Just TileClosed{mark=MarkFlag} -> do
        #field % ix2 posn % #mark .= MarkNone
        #stats % #flagCount %= (+ 1)
      Just TileClosed{} -> do
        #field % ix2 posn % #mark .= MarkFlag
        #stats % #flagCount %= subtract 1
      _ -> pure ()

gameMark :: Bool -> V2 Int -> GameM ()
gameMark user posn = do
  Game{options=Options{..}, ..} <- get
  when (state == GameRunning) $ do
    case field ^? ix2 posn of
      Just TileOpen{} | user -> gameChord True posn
      Just TileClosed{..} ->
        #field % ix2 posn % #mark .= case mark of
          MarkMark -> MarkNone
          MarkFlag -> MarkFlag
          _ -> MarkMark
      Nothing -> pure ()

gameChord :: Bool -> V2 Int -> GameM ()
gameChord user posn = do
  Game{options=Options{..}, ..} <- get
  case field ^? ix2 posn of
    Just TileOpen{..} | count > 0 -> do
      let (mines, opens) = partition snd
            [ (n, mark == MarkFlag) | n <- neighbors posn
            , Just t@TileClosed{..} <- pure (field ^? ix2 n) ]
      when (length mines == count) $ do
        mapM_ (gameOpen False . fst) opens
    _ -> pure ()

gameRestart :: GameM ()
gameRestart = do
  Game{options=Options{..}, ..} <- get
  time <- liftIO getCurrentTime
  #state .= GameStart
  #field .= emptyField height width
  #stats .= defaultGameStats time

mouseEvent :: Bool -> Name -> Maybe Vty.Button -> GameM ()
mouseEvent down (NameTile posn) (Just button) = do
  Game{options=Options{..}, ..} <- get
  when (down == mouseDrag) $ do
    lift $ Brick.invalidateCacheEntry NameField
    case button of
      Vty.BLeft -> gameOpen True posn
      Vty.BMiddle -> gameChord True posn
      Vty.BRight -> case field ^? ix2 posn of
        _ | not mouse -> gameFlag True posn
        Just TileClosed{mark=MarkFlag} -> pure ()
        Just _ -> gameFlag True posn
        Nothing -> pure ()
      _ -> pure ()
mouseEvent False NameStats _ = do
  lift $ Brick.invalidateCacheEntry NameField
  gameRestart
mouseEvent _ _ _ = pure ()

appDraw :: Game -> [Brick.Widget Name]
appDraw Game{options=Options{..}, ..} = [rendered] where
  rendered = Brick.hLimit (2 * width + 3)
    . Brick.joinBorders . Brick.border $ Brick.vBox
    [ Brick.clickable NameStats $ Brick.hBox
      [ Brick.str . show $ stats ^. #flagCount
      , Brick.txt "/" , Brick.str
        . show $ stats ^. #closedCount
      , Brick.padLeft (Brick.Pad 1) $ Brick.txt "✥"
        , Brick.str . show $ stats ^. #moveTimes
      , Brick.padLeft (Brick.Pad 1) $ Brick.txt "⊙"
        , Brick.str . show $ stats ^. #openTimes
      , Brick.padLeft (Brick.Pad 1) $ Brick.txt "⚑"
        , Brick.str . show $ stats ^. #flagTimes
      , Brick.padLeft Brick.Max . Brick.str
        . formatTime defaultTimeLocale "%1Es"
        $ let GameStats{..} = stats in diffUTCTime timeNow timeStart
      ]
    , Brick.hBorder
    , Brick.cached NameField . Brick.vBox
      . zipWith drawRow [0..] $ toList field
    ]
  bgAttr :: Bool -> Brick.AttrName
  bgAttr open = Brick.attrName $ if open then "open" else "closed"
  tileSep :: Bool -> Bool -> Brick.Widget Name
  tileSep l r = Brick.withAttr (bgAttr l <> bgAttr r) (Brick.txt "▌")
  drawRow :: Int -> Seq Tile -> Brick.Widget Name
  drawRow r row = Brick.hBox (drawCols r 0 True (toList row))
  drawCols :: Int -> Int ->  Bool -> [Tile] -> [Brick.Widget Name]
  drawCols row col last [] = [tileSep last True]
  drawCols row col last (tile:tiles) = tileSep last next
    : curs (drawTile row col tile) : drawCols row (col + 1) next tiles
    where
    next = case tile of TileOpen{} -> True ; _ -> False
    curs = if cursor /= V2 row col then id
      else Brick.showCursor NameCursor (Brick.Location (0, 0))
  drawTile :: Int -> Int -> Tile -> Brick.Widget Name
  drawTile row col tile = sym $ case tile of
    TileClosed _ MarkNone -> "closed"
    TileClosed _ MarkFlag -> "flag"
    TileClosed _ MarkMark -> "mark"
    TileClosed _ MarkMine -> "mine"
    TileClosed _ MarkWrong -> "wrong"
    TileOpen n -> words "zero one two three four five six seven eight" !! n
    where
    sym s = Brick.clickable (NameTile (V2 row col))
      $ Brick.withAttr (Brick.attrName s)
      . Brick.txt . Text.singleton $ symbols Map.! s

appHandleEvent :: Brick.BrickEvent Name () -> Brick.EventM Name Game ()
appHandleEvent event = do
  Game{options=Options{..}, ..} <- get
  let lookupKey (Vty.EvKey k ms) = keys Map.!? (k, ms)
      lookupKey _ = Nothing
  case event of
    Brick.VtyEvent (lookupKey -> Just key) -> do
      Brick.invalidateCacheEntry NameField
      case key of
        "quit" -> Brick.halt
        "up"    -> runGame . gameMove False $ V2 (-1)  0
        "down"  -> runGame . gameMove False $ V2   1   0
        "left"  -> runGame . gameMove False $ V2   0 (-1)
        "right" -> runGame . gameMove False $ V2   0   1
        "open-up"    -> runGame . gameMove True $ V2 (-1)  0
        "open-down"  -> runGame . gameMove True $ V2   1   0
        "open-left"  -> runGame . gameMove True $ V2   0 (-1)
        "open-right" -> runGame . gameMove True $ V2   0   1
        "jump-up"    -> runGame . gameMove False $ V2 (-jump)  0
        "jump-down"  -> runGame . gameMove False $ V2   jump   0
        "jump-left"  -> runGame . gameMove False $ V2   0 (-jump)
        "jump-right" -> runGame . gameMove False $ V2   0   jump
        "open" -> runGame $ gameOpen True cursor
        "flag" -> runGame $ gameFlag True cursor
        "mark" -> runGame $ gameMark True cursor
        "restart" -> runGame gameRestart
        _ -> pure ()
    Brick.VtyEvent e -> pure ()
    Brick.AppEvent e -> do
      when (state == GameRunning) $ do
        time <- liftIO getCurrentTime
        #stats % #timeNow .= time
    Brick.MouseDown n b ms l -> do
      runGame $ mouseEvent True n (Just b)
      #mouse .= True
    Brick.MouseUp n b l -> do
      runGame $ mouseEvent False n b
      #mouse .= False

appAttrMap :: Game -> Brick.AttrMap
appAttrMap Game{options=Options{..}, ..} = attrs where
  withColor fg = if fg then Vty.withForeColor else Vty.withBackColor
  mkColor n = if n < 16 then Vty.ISOColor n else Vty.Color240 (n - 16)
  colorOf = (cs Map.!) where cs = Map.map (mkColor . fromIntegral) colors
  attrs = Brick.attrMap Vty.defAttr $
    [ ( Brick.attrName name , colorOf name
      `Brick.on` colorOf (if open then "open" else "closed") )
    | (name, open) <-
      [ ("zero",   True),  ("one",    True),  ("two",    True)
      , ("three",  True),  ("four",   True),  ("five",   True)
      , ("six",    True),  ("seven",  True),  ("eight",  True)
      , ("flag",   False), ("mine",   False), ("mark",   False)
      , ("wrong",  False) ] ] ++
    [ ( Brick.attrName x, colorOf y `Brick.on` colorOf x )
    | (x, y) <- [("open",   "closed"),  ("closed", "open")] ] ++
    [ ( lAttr <> rAttr, lColor `Brick.on` rColor )
    | let getPair n = (Brick.attrName n, colorOf n)
    , [(lAttr, lColor), (rAttr, rColor)] <-
      replicateM 2 (map getPair ["open", "closed"]) ]

appStartEvent :: Brick.EventM Name Game ()
appStartEvent = do
  vty <- Brick.getVtyHandle
  liftIO $ Vty.setMode (Vty.outputIface vty) Vty.Mouse True
  runGame gameRestart

main = do
  opts@Options{..} <- Opt.execParser $
    Opt.info (Opt.helper <*> optionsParser) Opt.fullDesc
  hSetBuffering stderr NoBuffering
  random <- initStdGen >> getStdGen
  time <- getCurrentTime
  print time
  mapM_ print (Map.toList symbols)
  chan <- Brick.newBChan 10
  void . forkIO . forever $ do
    Brick.writeBChan chan ()
    threadDelay 50_000
  let buildVty = Vty.mkVty Vty.defaultConfig
  vty <- buildVty
  void $ Brick.customMain vty buildVty (Just chan)
    Brick.App
      { appDraw         = appDraw
      , appChooseCursor = Brick.showFirstCursor
      , appHandleEvent  = appHandleEvent
      , appStartEvent   = appStartEvent
      , appAttrMap      = appAttrMap }
    Game
      { state   = GameStart
      , field   = Seq.empty
      , options = opts
      , cursor  = flip div 2 <$> V2 height width
      , mouse   = False
      , random  = random
      , stats   = defaultGameStats time
      }
