cmake_minimum_required (VERSION 3.22)

project(tiny-js CXX)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wno-conversion -Wno-sign-compare -Wno-float-conversion")

if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Debug")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
else()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3")
endif()

if (NOT WIN32)
	include(CheckCXXCompilerFlag)
	CHECK_CXX_COMPILER_FLAG("-std=c++2b" COMPILER_SUPPORTS_CXX23)
	if(COMPILER_SUPPORTS_CXX23)
    	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++2b")
	else()
    	message(FATAL_ERROR "Compiler ${CMAKE_CXX_COMPILER} has no C++23 features support.")
	endif()
endif()

include_directories(include)

ADD_EXECUTABLE(tiny-js-cli samples/Script.cpp)
ADD_EXECUTABLE(cpjs samples/ConstexprScript.cpp)

ADD_EXECUTABLE(tiny-js-tests tests/run_tests.cpp)

add_custom_command(
	TARGET tiny-js-tests POST_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory
			${CMAKE_SOURCE_DIR}/tests
			${CMAKE_CURRENT_BINARY_DIR}/tests)

ADD_EXECUTABLE(wasm_js wasm_js.cpp)
