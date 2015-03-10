find_package(Qt5Test REQUIRED)

if (ENABLE_SYSTEM_LAPACK)
  set(LAPACK_LIBS "lapack;blas")
else ()
  set(LAPACK_LIBS "lapack-3.2;lapack_ext-3.2;blas-3.2")
endif()

include_directories(${ANI_HEADERS_DIR} ${CMAKE_SOURCE_DIR}/source/q3mesh_builder)
link_directories(${ANI_LIBS_DIR})


set(q3ani2dtest_SRC
    q3ani2dtest.cpp
)
add_executable(q3ani2dtest ${q3ani2dtest_SRC})
qt5_use_modules(q3ani2dtest Test)
add_dependencies(q3ani2dtest ani2D)
target_link_libraries(q3ani2dtest
    q3mesh_builder
    aft2D-${ANI2D_VERSION}
    c2f2D-${ANI2D_VERSION}
    view2D-${ANI2D_VERSION}
    mba2D-${ANI2D_VERSION}
    ${LAPACK_LIBS}
    gfortran
    m)
add_test(NAME q3ani2dtest COMMAND q3ani2dtest)
