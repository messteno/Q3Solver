find_package(Qt5Test REQUIRED)

if (ENABLE_SYSTEM_LAPACK)
  set(LAPACK_LIBS "lapack;blas")
else ()
  set(LAPACK_LIBS "lapack-3.2;lapack_ext-3.2;blas-3.2")
endif()

include_directories(
    ${ANI_HEADERS_DIR} ${CMAKE_SOURCE_DIR}/source/q3ani2d
    ${ANI_HEADERS_DIR} ${CMAKE_SOURCE_DIR}/source/q3mesh
    ${ANI_HEADERS_DIR} ${CMAKE_SOURCE_DIR}/source/q3sceleton
    ${ANI_HEADERS_DIR} ${CMAKE_SOURCE_DIR}/source/q3boundary
    ${ANI_HEADERS_DIR} ${CMAKE_SOURCE_DIR}/source/q3plot
    ${ANI_HEADERS_DIR} ${CMAKE_SOURCE_DIR}/source/q3calc
)
link_directories(${ANI_LIBS_DIR})


set(q3ani2dtest_SRC
    q3ani2dtest.cpp
)
add_executable(q3ani2dtest ${q3ani2dtest_SRC})
qt5_use_modules(q3ani2dtest Test)
add_dependencies(q3ani2dtest ani2D)
target_link_libraries(q3ani2dtest
    q3ani2d
    m)
add_test(NAME q3ani2dtest COMMAND q3ani2dtest)


set(q3calctest_SRC
    q3calctest.cpp
)
add_executable(q3calctest ${q3calctest_SRC})
qt5_use_modules(q3calctest Test)
add_dependencies(q3calctest ani2D)
target_link_libraries(q3calctest
    q3calc
    q3mesh
    q3sceleton
    q3boundary
    q3plot
    m)
add_test(NAME q3calctest COMMAND q3calctest)
