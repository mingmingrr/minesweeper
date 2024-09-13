cmake_minimum_required(VERSION 3.15)

project(mines CXX)

set(CMAKE_BUILD_TYPE Debug)

set(CMAKE_CXX_FLAGS "-fdiagnostics-color=always")

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(CURSES_NEED_WIDE TRUE)
include(FindCurses)
include_directories(${CURSES_INCLUDE_DIR})

include(FetchContent)

FetchContent_Declare(
	tomlplusplus
	GIT_REPOSITORY https://github.com/marzer/tomlplusplus.git
	GIT_TAG        v3.4.0
)
FetchContent_MakeAvailable(tomlplusplus)

add_executable(mines
	mines.cpp)
target_link_libraries(mines
	tomlplusplus::tomlplusplus
	${NCURSES_LIBRARIES})

