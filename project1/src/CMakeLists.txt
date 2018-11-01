cmake_minimum_required(VERSION 3.10)
project(compiler-proj1)

add_executable(subc
        hash.c
        subc.yy.c)

target_include_directories(subc
        PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

target_link_libraries(subc fl)

add_custom_command(OUTPUT subc.yy.c
        COMMAND flex -o subc.yy.c ${CMAKE_CURRENT_SOURCE_DIR}/subc.l
        MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/subc.l
        )
