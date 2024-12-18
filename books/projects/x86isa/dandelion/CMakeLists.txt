cmake_minimum_required(VERSION 3.10)
project(dandelion C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

include(ExternalProject)

ExternalProject_Add(mbuild
    GIT_REPOSITORY    https://github.com/intelxed/mbuild.git
    GIT_TAG          main
    SOURCE_DIR       ${CMAKE_BINARY_DIR}/mbuild
    CONFIGURE_COMMAND ""
    BUILD_COMMAND    ""
    INSTALL_COMMAND  ""
    UPDATE_COMMAND   ""
)

ExternalProject_Add(xed
    DEPENDS          mbuild
    GIT_REPOSITORY    https://github.com/intelxed/xed.git
    GIT_TAG          main
    SOURCE_DIR       ${CMAKE_BINARY_DIR}/xed
    CONFIGURE_COMMAND ""
    BUILD_COMMAND    cd ${CMAKE_BINARY_DIR}/xed && 
                    python3 mfile.py --static
    INSTALL_COMMAND  ""
    UPDATE_COMMAND   ""
)

set(XED_INCLUDE_DIR ${CMAKE_BINARY_DIR}/xed/obj/wkit/include)
set(XED_LIB_DIR ${CMAKE_BINARY_DIR}/xed/obj)

add_executable(dandelion-record dandelion-record.c)
add_dependencies(dandelion-record xed)
target_include_directories(dandelion-record PRIVATE ${XED_INCLUDE_DIR})
target_link_directories(dandelion-record PRIVATE ${XED_LIB_DIR})
target_link_libraries(dandelion-record PRIVATE libxed.a)
