#include <variant>
#include <ctime>
#include <ostream>
#include <chrono>
#include <cstdlib>
#include <thread>
#include <string>
#include <iostream>
#include <map>
#include <vector>
#include <type_traits>
#include <set>
#include <algorithm>
#include <numeric>
#include <cmath>
#include <random>
#include <cassert>
#include <functional>
#include <shared_mutex>
#include <mutex>
#include <ncursesw/curses.h>

using namespace std::chrono_literals;

#define DEBUGTRACE_WIDE 1
#define DEBUGTRACE_OUTPUT_FILE "debug.fifo"
#include "trace.hpp"
namespace R = debugtrace;

template<typename... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<typename... Ts> overloaded(Ts...) -> overloaded<Ts...>;

template<typename T>
struct Vec2 {
	T x;
	T y;

	bool operator ==(const Vec2<T>& that)
		{ return this->x == that.x && this->y == that.y; }
	template<typename F>
	Vec2<decltype(F{}(T{}))> map(const F& func)
		{ return { func(this->x), func(this->y) }; }
};

#define Vec2_operator(op) \
	template<typename A, typename B> \
	Vec2<decltype(A{} op B{})> operator op(const Vec2<A>& a, const Vec2<B>& b) \
		{ return { a.x op b.x, a.y op b.y }; } \
	template<typename A, typename B> \
	Vec2<decltype(A{} op B{})> operator op(const Vec2<A>& a, const B& b) \
		{ return { a.x op b, a.y op b }; } \
	template<typename A, typename B> \
	Vec2<decltype(A{} op B{})> operator op(const A& a, const Vec2<B>& b) \
		{ return { a op b.x, a op b.y }; }
Vec2_operator(+)
Vec2_operator(-)
Vec2_operator(*)
Vec2_operator(/)
Vec2_operator(%)
Vec2_operator(^)
Vec2_operator(|)

struct CursesWindow {
	CursesWindow() {
		ESCDELAY = 10;
		initscr();
		cbreak();
		noecho();
		curs_set(false);
		keypad(stdscr, true);
		mousemask(ALL_MOUSE_EVENTS | REPORT_MOUSE_POSITION, NULL);
		start_color();
		clear();
	}
	~CursesWindow() {
		keypad(stdscr, false);
		echo();
		nocbreak();
		endwin();
	}
};

struct CursesNoDelay {
	CursesNoDelay() { nodelay(stdscr, true); }
	~CursesNoDelay() { nodelay(stdscr, false); }
};

struct CursesUnderline {
	bool enable;
	CursesUnderline(bool enable) : enable(enable)
		{ if(enable) attron(A_UNDERLINE); }
	~CursesUnderline()
		{ if(this->enable) attroff(A_UNDERLINE); }
};

using Name = std::string;
const std::set<Name> symbols = {
	"Zero", "One", "Two",
	"Three", "Four", "Five",
	"Six", "Seven", "Eight",
	"Flag", "Mine", "Mark",
	"Wrong", "Closed",
};
const std::vector<Name> numbers = {
	"Zero", "One", "Two",
	"Three", "Four", "Five",
	"Six", "Seven", "Eight",
};

int index(const Name& sym)
	{ return std::distance(symbols.begin(), symbols.find(sym)); }

std::tuple<int,int,int> color(int n) {
	return { (n >> 16) * 999 / 0xff,
		((n >> 8) & 0xff) * 999 / 0xff,
		(n & 0xff) * 999 / 0xff };
}

using Action = std::string;
using Key = std::variant<int, std::string>;

struct Symbol {
	std::wstring repr;
	int foreground;
	int background;
};

struct Config {
	std::map<Name, Symbol> symbols;
	std::map<Key, Action> keys;
	int jump;
	int width;
	int height;
	int mines;
	bool autoChord;
	bool autoFlag;
	bool mouseDrag;
	int gridWidth;
};

struct Stats {
	std::chrono::time_point<std::chrono::steady_clock> timeStart;
	std::chrono::time_point<std::chrono::steady_clock> timeNow;
	int openTimes;
	int flagTimes;
	int chordTimes;
	int moveTimes;
	int open3BV;
	int total3BV;
	int flagCount;
	int closedCount;
};

enum struct State {
	Start,
	Running,
	Finished,
	Exploded,
	Quit,
};

enum struct Mark {
	None, Flag, Mark,
	Mine, Wrong,
};

struct Tile {
	bool open;
	bool mine;
	char count;
	Mark mark;
};

using Field = std::vector<std::vector<Tile>>;

struct Game {
	std::shared_mutex mutex;
	State state;
	Field field;
	Config config;
	Vec2<int> cursor;
	bool mouse;
	Stats stats;
};

Key getkey() {
	int head = getch();
	if(head != 0x1b) return head;
	std::string bits = "\x1b";
	CursesNoDelay _nodelay;
	int csi = getch();
	if(csi == -1) return bits;
	bits.push_back(csi);
	if(csi != '[') return bits;
	int bit = getch();
	while('0' <= bit && bit <= '?')
		{ bits.push_back(bit); bit = getch(); }
	while(' ' <= bit && bit <= '/')
		{ bits.push_back(bit); bit = getch(); }
	if('@' <= bit && bit <= '~')
		{ bits.push_back(bit); bit = getch(); }
	if(bit != -1) ungetch(bit);
	return bits;
}

void check_keys(
	std::string msg,
	const std::set<std::string>& xs,
	const std::set<std::string>& ys
) {
	std::vector<std::string> diff;
	std::set_difference(xs.begin(), xs.end(),
		ys.begin(), ys.end(), std::back_inserter(diff));
	if(diff.empty()) return;
	for(const auto& key: diff) {
		msg.push_back(' ');
		msg.append(key);
	}
	throw std::invalid_argument(msg);
}

void footer(Game& game) {
	std::wostringstream out;
	switch(game.state) {
		case State::Start: [[fallthrough]];
		case State::Running: out << L"🙂"; break;
		case State::Finished: out << L"😎"; break;
		case State::Exploded: out << L"😵"; break;
		case State::Quit: return;
	}
	const auto& stats = game.stats;
	std::chrono::duration<double> time = stats.timeNow - stats.timeStart;
	out << L" 🕖" << std::fixed << std::setprecision(1) << time.count()
		<< L" 🖱" << stats.openTimes << L" 🚩" << stats.flagTimes
		<< L" 🎜" << stats.chordTimes << L" ❔" << stats.markTimes;
	auto str = out.str();
	color_set(0, NULL);
	move(game.config.height, 0);
	clrtoeol();
	ins_nwstr(str.c_str(), str.size());
}

void draw(Game& game) {
	for(int r = 0; r < game.config.height; ++r)
		for(int c = 0; c < game.config.width; ++c) {
			auto& tile = game.field[r][c];
			std::string symbol;
			switch(tile.mark) {
				case Mark::None:  symbol =
					tile.open ? numbers[tile.count] : "Closed"; break;
				case Mark::Flag:  symbol = "Flag"; break;
				case Mark::Mark:  symbol = "Mark"; break;
				case Mark::Mine:  symbol = "Mine"; break;
				case Mark::Wrong: symbol = "Wrong"; break;
				default: throw std::logic_error("unreachable");
			}
			CursesUnderline _underline(game.cursor == Vec2<int>{r, c});
			color_set(index(symbol) + 1, NULL);
			auto& repr = game.config.symbols[symbol].repr;
			mvins_nwstr(r, c * game.config.gridWidth, repr.c_str(), repr.size());
		}
	footer(game);
}

void update(Game* game) {
	while(game->state != State::Quit) {
		std::this_thread::sleep_for(50ms);
		std::unique_lock _lock(game->mutex);
		if(game->state != State::Running) continue;
		game->stats.timeNow = std::chrono::steady_clock::now();
		footer(*game);
		refresh();
	}
}

inline Tile& index(Game& game, Vec2<int> posn)
	{ return game.field[posn.x][posn.y]; }
inline Tile& index(Game& game)
	{ return index(game, game.cursor); }

template<typename F>
void neighbors(Game& game, Vec2<int> posn, F func) {
	const auto r1 = std::min(game.config.height - 1, posn.x + 1);
	const auto c1 = std::min(game.config.width - 1, posn.y + 1);
	for(int r = std::max(0, posn.x - 1); r <= r1; ++r)
		for(int c = std::max(0, posn.y - 1); c <= c1; ++c)
			if(r != posn.x || c != posn.y)
				func(Vec2<int>{ r, c }, game.field[r][c]);
}

template<typename F>
void alltiles(Game& game, F func) {
	for(int r = 0; r < game.config.height; ++r)
		for(int c = 0; c < game.config.width; ++c)
			func(Vec2<int>{ r, c }, game.field[r][c]);
}

struct GameException : public std::exception
	{ using std::exception::exception; };
struct OpenMineTile : public GameException
	{ using GameException::GameException; };
struct AllTilesOpened : public GameException
	{ using GameException::GameException; };
struct QuitGame : public GameException
	{ using GameException::GameException; };

void generate(Game& game);

void chord(Game& game, Vec2<int> posn, bool user) {
	if(user && game.state == State::Running)
		game.stats.chordTimes += 1;
	const auto& tile = index(game, posn);
	if(!tile.open) return;
	if(tile.count == 0) return;
	int count = 0;
	neighbors(game, game.cursor, [&](const auto& _, auto& tile)
		{ if(tile.mark == Mark::Flag) count += 1; });
	if(count != tile.count) return;
	neighbors(game, game.cursor, [&](const auto& posn, auto& tile)
		{ if(tile.mark != Mark::Flag) open(game, posn, false); });
}

void open1(Game& game, Vec2<int> posn) {
	auto& tile = index(game, posn);
	if(tile.open) return;
	if(tile.mark == Mark::Flag) return;
	if(tile.mine) {
		game.state = State::Exploded;
		throw OpenMineTile();
	}
	tile.open = true;
	tile.mark = Mark::None;
	game.stats.closedCount -= 1;
	if(game.stats.closedCount == 0) {
		game.state = State::Finished;
		throw AllTilesOpened();
	}
	if(tile.count == 0) neighbors(game, posn,
		[&](const auto& posn, const auto& tile) { open1(game, posn); });
}

void open(Game& game, Vec2<int> posn, bool user) {
	const auto& tile = index(game, posn);
	switch(game.state) {
		case State::Start: generate(game); break;
		case State::Running:
			if(!tile.open) {
				if(user) game.stats.openTimes += 1;
				open1(game, posn);
			} else if(user)
				chord(game, posn, user);
			break;
		default: break;
	}
}

void flag(Game& game, Vec2<int> posn, bool user) {
	if(game.state != State::Running) return;
	auto& tile = index(game, posn);
	if(tile.open && user) {
		chord(game, posn, user);
		return;
	}
	if(user) game.stats.flagTimes += 1;
	if(!tile.open && tile.mark == Mark::Flag && user)
		tile.mark = Mark::None;
	else if(!tile.open && tile.mark != Mark::Flag)
		tile.mark = Mark::Flag;
}

void generate(Game& game) {
	game.state = State::Running;
	std::vector<Vec2<int>> mines, posns;
	alltiles(game, [&](const auto& posn, auto& tile) {
		tile = Tile {
			.open = false,
			.mine = false,
			.count = 0,
			.mark = Mark::None
		};
		if(!(posn.x - 1 <= game.cursor.x && game.cursor.x <= posn.x + 1
			&& posn.y - 1 <= game.cursor.y && game.cursor.y <= posn.y + 1))
			posns.push_back(posn);
	});
	std::sample(posns.begin(), posns.end(),
		std::back_inserter(mines), game.config.mines,
		std::mt19937{std::random_device{}()});
	for(const auto& [r, c]: mines) {
		game.field[r][c].mine = true;
		for(int a = r - 1; a <= r + 1; ++a)
			for(int b = c - 1; b <= c + 1; ++b) {
				if(!(0 <= a && a < game.config.height
					&& 0 <= b && b < game.config.width)) continue;
				game.field[a][b].count += 1;
			}
	}
	std::wostringstream repr;
	for(int r = 0; r < game.config.height; ++r) {
		for(int c = 0; c < game.config.width; ++c) {
			if(game.cursor == Vec2<int>{r, c})
				repr << '>';
			else
				repr << ' ';
			auto& tile = game.field[r][c];
			repr << " 123456789"[tile.count];
			if(tile.mine)
				repr << 'X';
			else
				repr << L'·';
		}
		repr << std::endl;
	}
	R::trace(repr.str());
	const auto time = std::chrono::steady_clock::now();
	game.stats.timeStart = time;
	game.stats.timeNow = time;
	open(game, game.cursor, true);
}

void restart(Game& game) {
	game.state = State::Start;
	game.field.resize(game.config.height);
	for(int r = 0; r < game.config.height; ++r) {
		game.field[r].resize(game.config.width);
		for(int c = 0; c < game.config.width; ++c) {
			game.field[r][c] = Tile {
				.open = false,
				.mine = false,
				.count = 0,
				.mark = Mark::None
			};
		}
	}
	const auto time = std::chrono::steady_clock::now();
	game.stats = Stats {
		.timeStart = time,
		.timeNow = time,
		.openTimes = 0,
		.flagTimes = 0,
		.chordTimes = 0,
		.moveTimes = 0,
		.open3BV = 0,
		.total3BV = 0,
		.flagCount = 0,
		.closedCount = game.config.width * game.config.height - game.config.mines
	};
}

void move(Game& game, Vec2<int> dirn) {
	game.cursor.x = std::clamp(game.cursor.x + dirn.x, 0, game.config.height - 1);
	game.cursor.y = std::clamp(game.cursor.y + dirn.y, 0, game.config.width - 1);
}

std::map<Action, std::function<void(Game&)>> actions = {
	{ "Up",    [](Game& game) { move(game, {-1, 0}); } },
	{ "Down",  [](Game& game) { move(game, { 1, 0}); } },
	{ "Left",  [](Game& game) { move(game, {0, -1}); } },
	{ "Right", [](Game& game) { move(game, {0,  1}); } },
	{ "OpenUp",    [](Game& game) { move(game, {-1, 0}); open(game, game.cursor, true); } },
	{ "OpenDown",  [](Game& game) { move(game, { 1, 0}); open(game, game.cursor, true); } },
	{ "OpenLeft",  [](Game& game) { move(game, {0, -1}); open(game, game.cursor, true); } },
	{ "OpenRight", [](Game& game) { move(game, {0,  1}); open(game, game.cursor, true); } },
	{ "JumpUp",    [](Game& game) { move(game, {-game.config.jump, 0}); } },
	{ "JumpDown",  [](Game& game) { move(game, { game.config.jump, 0}); } },
	{ "JumpLeft",  [](Game& game) { move(game, {0, -game.config.jump}); } },
	{ "JumpRight", [](Game& game) { move(game, {0,  game.config.jump}); } },
	{ "Flag",    [](Game& game) { flag(game, game.cursor, true); } },
	{ "Mark",    [](Game& game) { } },
	{ "Open",    [](Game& game) { open(game, game.cursor, true); } },
	{ "Restart", [](Game& game) { restart(game); } },
	{ "Quit",    [](Game& game) { throw QuitGame(); } },
};

int main() {
	R::trace(R::now(), "========================================");
	std::locale::global(std::locale(""));
	srand(time(NULL));

	Game game = {
		.state = State::Start,
		.config = Config {
			.symbols = {
				{ "Zero",   { L"　", 255, 232 } },
				{ "One",    { L"１", 105, 232 } },
				{ "Two",    { L"２", 120, 232 } },
				{ "Three",  { L"３", 210, 232 } },
				{ "Four",   { L"４",  21, 232 } },
				{ "Five",   { L"５", 196, 232 } },
				{ "Six",    { L"６",  33, 232 } },
				{ "Seven",  { L"７", 255, 232 } },
				{ "Eight",  { L"８", 248, 232 } },
				{ "Flag",   { L"🚩", 196, 238 } },
				{ "Mine",   { L"💥", 214, 238 } },
				{ "Mark",   { L"❔", 226, 238 } },
				{ "Wrong",  { L"❌", 201, 238 } },
				{ "Closed", { L"· ", 255, 238 } },
			},
			.keys = {
				{ 'u', "Up" },
				{ 'j', "Down" },
				{ 'h', "Left" },
				{ 'k', "Right" },
				{ KEY_UP,    "Up" },
				{ KEY_DOWN,  "Down" },
				{ KEY_LEFT,  "Left" },
				{ KEY_RIGHT, "Right" },
				{ 'U', "OpenUp" },
				{ 'J', "OpenDown" },
				{ 'H', "OpenLeft" },
				{ 'K', "OpenRight" },
				{ KEY_SR,     "OpenUp" },
				{ KEY_SF,     "OpenDown" },
				{ KEY_SLEFT,  "OpenLeft" },
				{ KEY_SRIGHT, "OpenRight" },
				{ 579/*kUP3*/,  "JumpUp" },
				{ 538/*kDN3*/,  "JumpDown" },
				{ 558/*kLFT3*/, "JumpLeft" },
				{ 573/*kRIT3*/, "JumpRight" },
				{ 581/*kUP5*/,  "JumpUp" },
				{ 540/*kDN5*/,  "JumpDown" },
				{ 560/*kLFT5*/, "JumpLeft" },
				{ 575/*kRIT5*/, "JumpRight" },
				{ 'x', "Flag" },
				{ 'c', "Mark" },
				{ 'z', "Open" },
				{ ' ', "Open" },
				{ 'r', "Restart" },
				{ KEY_F(2), "Restart" },
				{ 'q', "Quit" },
				{ 0x1b, "Quit" },
			},
			.jump = 5,
			.width = 30,
			.height = 16,
			.mines = 99,
			.autoChord = false,
			.autoFlag = false,
			.mouseDrag = true,
		},
		.mouse = false,
	};

	{
		std::set<std::string> actions_set;
		for(const auto& [ x, _ ]: actions) actions_set.insert(x);
		std::set<std::string> config_check;
		for(const auto& [ _, x ]: game.config.keys) config_check.insert(x);
		check_keys("unknown keys:", config_check, actions_set);
		check_keys("missing keys:", actions_set, config_check);
		config_check.clear();
		for(const auto& [ x, _ ]: game.config.symbols) config_check.insert(x);
		check_keys("unknown symbols:", config_check, symbols);
		check_keys("missing symbols:", symbols, config_check);
		config_check.clear();
	}

	game.config.gridWidth = std::accumulate(
		game.config.symbols.begin(), game.config.symbols.end(), 0,
		[](int x, const auto& y) { return std::max(x,
			wcswidth(y.second.repr.c_str(), y.second.repr.size())); });
	for(auto& [ _, x ]: game.config.symbols)
		x.repr.insert(x.repr.end(), game.config.gridWidth
			- wcswidth(x.repr.c_str(), x.repr.size()), ' ');
	game.cursor = Vec2<int>{game.config.height, game.config.width} / 2;
	restart(game);

	CursesWindow _cwin;

	for(auto& [ name, sym ]: game.config.symbols)
		init_pair(index(name) + 1, sym.foreground, sym.background);

	draw(game);
	std::thread(update, &game).detach();

	while(true) {
		auto key = getkey();
		std::visit(overloaded {
			[&](int key) { R::trace(R::now(), keyname(key)); },
			[&](std::string key) {
				key.erase(0, 1);
				key.insert(0, "\\x1b");
				R::trace(R::now(), R::wconvert<std::string, std::wstring>(std::move(key)));
			},
		}, key);
		auto action = game.config.keys.find(key);
		std::unique_lock _lock(game.mutex);
		if(action != game.config.keys.end()) {
			try {
				actions[action->second](game);
			} catch(const QuitGame& exc) {
				game.state = State::Quit;
				break;
			} catch(const GameException& exc) {
				alltiles(game, [&](const auto& posn, auto& tile) {
					if(tile.open && tile.mine)
						tile.mark = Mark::Mine;
					else if(!tile.mine && tile.mark == Mark::Flag)
						tile.mark = Mark::Wrong;
					else if(tile.mine && tile.mark != Mark::Flag)
						tile.mark = Mark::Mine;
				});
			}
		}
		erase();
		draw(game);
		refresh();
	}
}

