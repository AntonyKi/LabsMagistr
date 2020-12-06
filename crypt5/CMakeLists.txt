cmake_minimum_required(VERSION 3.17)
project(crypt5)

set(CMAKE_CXX_STANDARD 14)

add_executable(crypt5 main.cpp Point.cpp Point.h diffie_hellman.cpp diffie_hellman.h BinaryPolynomial.cpp BinaryPolynomial.h)