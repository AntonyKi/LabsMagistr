cmake_minimum_required(VERSION 3.17)
project(crypt1)

set(CMAKE_CXX_STANDARD 14)

add_executable(crypt1 main.cpp aes.cpp aes.h operations.cpp operations.h)