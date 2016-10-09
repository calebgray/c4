cmake_minimum_required(VERSION 3.6)
project(c4)

SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c11 -pedantic -Wall -march=native -Ofast -pipe -fomit-frame-pointer -fno-stack-protector -ftree-loop-linear -ftree-loop-distribution -floop-block -floop-strip-mine -floop-nest-optimize")

set(C4_SOURCE_FILES
    c4.c)

set(HELLO_SOURCE_FILES
  hello.c)

add_executable(c4 ${C4_SOURCE_FILES})
target_link_libraries(c4 dl)

add_executable(hello ${HELLO_SOURCE_FILES})