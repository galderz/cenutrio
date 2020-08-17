cmake_minimum_required(VERSION 3.15)
project(cenutrio C)

set(CMAKE_C_STANDARD 99)

add_executable(hello hello/main.c)
add_executable(overview.harbison overview.harbison/startup.c overview.harbison/hello.h overview.harbison/hello.c)
add_executable(constants.harbison.strings constants.harbison/strings.c)
add_executable(constants.harbison.number-limits constants.harbison/number-limits.c)
add_executable(constants.harbison.long-rules constants.harbison/long-rules.c)

#find_library(FDLIBM_LIBRARY fdlibm /Users/g/1/jawa/substratevm/clibraries/target/clibraries/fdlibm)
#link_directories(/Users/g/1/jawa/substratevm/clibraries/target/clibraries)
#target_link_libraries(fdlibm LINK_PUBLIC ${FDLIBM_LIBRARY})

add_library(libfdlibm SHARED IMPORTED)
set_target_properties(libfdlibm PROPERTIES IMPORTED_LOCATION "/Users/g/1/jawa/substratevm/clibraries/target/clibraries/fdlibm/libm.a")
add_executable(fdlibm fdlibm/fdlibm.c)
include_directories("/Users/g/1/jawa/substratevm/clibraries/target/src/fdlibm")
target_link_libraries(fdlibm libfdlibm)

add_executable(e_log e_log/e_log.c)