###
# Copyright (c) 2014, Artem Meshcheryakov
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
###

cmake_minimum_required(VERSION 2.8.11)

# Default configuration values. These must be before the project command or
# If no build type is specified, default to "Release"
set(CMAKE_BUILD_TYPE Debug)

project(q3solver)
set(PROJECT_LONGNAME "Q3Solver")
set(PROJECT_VERSION "0.0.1")

# Global CMake options
set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)

# Configure Qt
find_package(Qt5Widgets)

include(ExternalProject)

set(PATCH_DIR ${CMAKE_SOURCE_DIR}/patch)
set(3RDPARTY_DIR ${CMAKE_SOURCE_DIR}/3rdparty)

set(ANI2D_VERSION 3.0)
set(ANI2D_PROJECT ani2D-${ANI2D_VERSION})
set(ANI_PREFIX ${3RDPARTY_DIR}/${ANI2D_PROJECT})
set(ANI_SRC_DIR ${ANI_PREFIX}/ani2D)
set(ANI_TMP_DIR ${ANI_PREFIX}/tmp)
set(ANI_BINARY_DIR ${ANI_SRC_DIR}/build)
set(ANI_HEADERS_DIR ${ANI_SRC_DIR}/include)
set(ANI_LIBS_DIR ${ANI_SRC_DIR}/lib)
ExternalProject_Add(ani2D
    PREFIX ${ANI_ROOT_DIR}
    DOWNLOAD_DIR ${ANI_PREFIX}
    SOURCE_DIR ${ANI_SRC_DIR}
    BINARY_DIR ${ANI_BINARY_DIR}
    TMP_DIR ${ANI_TMP_DIR}
    URL http://sourceforge.net/projects/ani2d/files/ani2d/${ANI2D_PROJECT}/${ANI2D_PROJECT}.tar.gz
    URL_MD5 3a0f5942d8466ccb86ed87de5aa56a23
    PATCH_COMMAND patch -p1 < ${PATCH_DIR}/ani2d_0001.patch
)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)
add_subdirectory(source)

