if (ENABLE_SYSTEM_LAPACK)
  set(LAPACK_LIBS "lapack;blas")
else ()
  set(LAPACK_LIBS "lapack-3.2;lapack_ext-3.2;blas-3.2")
endif()

include_directories(${ANI_HEADERS_DIR} ${CMAKE_SOURCE_DIR}/source/q3mesh ${CMAKE_SOURCE_DIR}/source/q3sceleton ${CMAKE_SOURCE_DIR}/source/q3plot ${CMAKE_SOURCE_DIR}/source/q3boundary)
link_directories(${ANI_LIBS_DIR})

aux_source_directory(. q3ani2d_SRC)
FILE(GLOB q3ani2d_UI_list "*.ui")

qt5_wrap_ui(q3ani2d_UI
    ${q3ani2d_UI_list})

add_library(q3ani2d STATIC
    ${q3ani2d_SRC}
    ${q3ani2d_UI})

qt5_use_modules(q3ani2d Widgets)
add_dependencies(q3ani2d ani2D)
target_link_libraries(q3ani2d
    aft2D-${ANI2D_VERSION}
    c2f2D-${ANI2D_VERSION}
    view2D-${ANI2D_VERSION}
    mba2D-${ANI2D_VERSION}
    ${LAPACK_LIBS}
    gfortran
    q3plot
    q3mesh
    q3sceleton
    q3boundary
    m)

