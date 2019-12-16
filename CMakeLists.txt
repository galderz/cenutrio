cmake_minimum_required(VERSION 3.15)
project(cenutrio C)

set(CMAKE_C_STANDARD 99)

add_executable(hello hello/main.c)
add_executable(overview_harbison overview.harbison/startup.c overview.harbison/hello.h overview.harbison/hello.c)
