cmake_minimum_required(VERSION 3.15)
project(cenutrio C)

set(CMAKE_C_STANDARD 99)

add_executable(hello hello/main.c)
add_executable(overview.harbison overview.harbison/startup.c overview.harbison/hello.h overview.harbison/hello.c)
add_executable(constants.harbison.strings constants.harbison/strings.c)
add_executable(constants.harbison.number-limits constants.harbison/number-limits.c)
add_executable(constants.harbison.long-rules constants.harbison/long-rules.c)
