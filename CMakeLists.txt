cmake_minimum_required(VERSION 3.6)
project(sscc)

#
# Custom Build Types
#
SET(CMAKE_C_FLAGS_INSANE "-std=c11 -pedantic -Wall -march=native -Ofast -pipe -fomit-frame-pointer -fno-stack-protector -ftree-loop-linear -ftree-loop-distribution -floop-block -floop-strip-mine -floop-nest-optimize" CACHE STRING "Flags used by the C compiler during insane builds." FORCE)
SET(CMAKE_CXX_FLAGS_INSANE "${CMAKE_C_FLAGS_INSANE}" CACHE STRING "Flags used by the C++ compiler during insane builds." FORCE)
SET(CMAKE_EXE_LINKER_FLAGS_INSANE "" CACHE STRING "Flags used for linking binaries during insane builds." FORCE)
SET(CMAKE_SHARED_LINKER_FLAGS_INSANE "" CACHE STRING "Flags used by the shared libraries linker during insane builds." FORCE)
MARK_AS_ADVANCED(CMAKE_CXX_FLAGS_INSANE CMAKE_C_FLAGS_INSANE CMAKE_EXE_LINKER_FLAGS_INSANE CMAKE_SHARED_LINKER_FLAGS_INSANE)

set(SSCC_SOURCE_FILES
  sscc.c)

add_executable(sscc ${SSCC_SOURCE_FILES})
target_link_libraries(sscc dl)

set(C5_SOURCE_FILES
  c5.c)

add_executable(c5 ${C5_SOURCE_FILES})
target_link_libraries(c5 dl)

set(HELLO_SOURCE_FILES
  hello.c)

add_executable(hello ${HELLO_SOURCE_FILES})