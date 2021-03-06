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
#set(CMAKE_BUILD_TYPE Release)

project(q3solver)
set(PROJECT_LONGNAME "Q3Solver")
set(PROJECT_VERSION "0.0.1")

# Global CMake options
set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)

# set(CMAKE_BUILD_TYPE Release)

set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/CMake)
include(PCHSupport)

# Configure Qt
find_package(Qt5Widgets)
find_package(Qt5PrintSupport REQUIRED)

include(ExternalProject)

set(3RDPARTY_DIR ${CMAKE_SOURCE_DIR}/3rdparty)
set(3RDPARTY_BUILD_DIR ${3RDPARTY_DIR}/build)
set(3RDPARTY_HEADERS_DIR ${3RDPARTY_DIR}/include)
set(3RDPARTY_LIBS_DIR ${3RDPARTY_DIR}/lib)

set(PATCH_DIR ${CMAKE_SOURCE_DIR}/patch)

set(ANI2D_VERSION 3.0)
set(ANI2D_PROJECT ani2D-${ANI2D_VERSION})
set(ANI_PREFIX ${3RDPARTY_BUILD_DIR}/${ANI2D_PROJECT})
set(ANI_SRC_DIR ${ANI_PREFIX}/ani2D)
set(ANI_TMP_DIR ${ANI_PREFIX}/tmp)
set(ANI_BINARY_DIR ${ANI_SRC_DIR}/build)
set(ANI_HEADERS_DIR ${ANI_SRC_DIR}/include)
set(ANI_LIBS_DIR ${ANI_SRC_DIR}/lib)
ExternalProject_Add(ani2D
    DOWNLOAD_DIR ${ANI_PREFIX}
    SOURCE_DIR ${ANI_SRC_DIR}
    BINARY_DIR ${ANI_BINARY_DIR}
    TMP_DIR ${ANI_TMP_DIR}
    URL http://sourceforge.net/projects/ani2d/files/ani2d/${ANI2D_PROJECT}/${ANI2D_PROJECT}.tar.gz
    URL_MD5 3a0f5942d8466ccb86ed87de5aa56a23
    PATCH_COMMAND patch -p1 < ${PATCH_DIR}/ani2d_0001.patch
)

set(QCUSTOM_PLOT_VERSION 1.3.1)
set(QCUSTOM_PLOT_PROJECT qcustomplot-${QCUSTOM_PLOT_VERSION})
set(QCUSTOM_PLOT_PREFIX ${3RDPARTY_BUILD_DIR}/${QCUSTOM_PLOT_PROJECT})
set(QCUSTOM_PLOT_SRC_DIR ${QCUSTOM_PLOT_PREFIX}/qcustomplot)
set(QCUSTOM_PLOT_LIB_SRC_DIR ${QCUSTOM_PLOT_SRC_DIR}/qcustomplot-sharedlib)
set(QCUSTOM_PLOT_LIB_BINARY_DIR ${QCUSTOM_PLOT_LIB_SRC_DIR}/sharedlib-compilation/)
set(QCUSTOM_PLOT_TMP_DIR ${QCUSTOM_PLOT_PREFIX}/tmp)
set(QCUSTOM_PLOT_LIB libqcustomplot.so.${QCUSTOM_PLOT_VERSION})
set(QCUSTOM_PLOT_LIBS_DIR ${QCUSTOM_PLOT_PREFIX}/lib)
set(QCUSTOM_PLOT_HEADERS_DIR ${QCUSTOM_PLOT_PREFIX}/include)
ExternalProject_Add(qcustomplot
    DOWNLOAD_DIR ${QCUSTOM_PLOT_PREFIX}
    SOURCE_DIR ${QCUSTOM_PLOT_SRC_DIR}
    INSTALL_DIR ${QCUSTOM_PLOT_HEADERS_DIR}
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND cp ${QCUSTOM_PLOT_SRC_DIR}/qcustomplot.h ${QCUSTOM_PLOT_HEADERS_DIR}
    URL http://www.qcustomplot.com/release/${QCUSTOM_PLOT_VERSION}/QCustomPlot.tar.gz
    URL_MD5 84b525f69fad67d8a77e7994891c2880
)
ExternalProject_Add(qcustomplot-sharedlib
    DEPENDS qcustomplot
    DOWNLOAD_DIR ${QCUSTOM_PLOT_PREFIX}
    SOURCE_DIR ${QCUSTOM_PLOT_LIB_SRC_DIR}
    BINARY_DIR ${QCUSTOM_PLOT_LIB_BINARY_DIR}
    INSTALL_DIR ${QCUSTOM_PLOT_LIBS_DIR}
    CONFIGURE_COMMAND qmake
    BUILD_COMMAND make
    INSTALL_COMMAND cp ${QCUSTOM_PLOT_LIB_BINARY_DIR}/${QCUSTOM_PLOT_LIB} ${QCUSTOM_PLOT_LIBS_DIR}/libqcustomplot.so.1
    URL http://www.qcustomplot.com/release/${QCUSTOM_PLOT_VERSION}/QCustomPlot-sharedlib.tar.gz
    URL_MD5 05c3c32a28116b63e09213c8ed2b7055
)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)
add_subdirectory(source)

