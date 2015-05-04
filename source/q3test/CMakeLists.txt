find_package(Qt5Test REQUIRED)

include_directories(
    ${ANI_HEADERS_DIR}
    ${QCUSTOM_PLOT_HEADERS_DIR}
    ${CMAKE_SOURCE_DIR}/source/q3ani2d
    ${CMAKE_SOURCE_DIR}/source/q3mesh
    ${CMAKE_SOURCE_DIR}/source/q3sceleton
    ${CMAKE_SOURCE_DIR}/source/q3boundary
    ${CMAKE_SOURCE_DIR}/source/q3plot
    ${CMAKE_SOURCE_DIR}/source/q3calc
)
link_directories(${ANI_LIBS_DIR} ${QCUSTOM_PLOT_LIBS_DIR})


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
target_link_libraries(q3calctest
    q3calc
    q3mesh
    q3sceleton
    q3boundary
    q3plot
    m)
add_test(NAME q3calctest COMMAND q3calctest)


# set(q3qcustomplottest_SRC
#     q3qcustomplottest.cpp
# )
# add_definitions(-DQCUSTOMPLOT_USE_LIBRARY)
# add_executable(q3qcustomplottest ${q3qcustomplottest_SRC})
# qt5_use_modules(q3qcustomplottest Test Widgets PrintSupport)
# add_dependencies(q3qcustomplottest qcustomplot-sharedlib)
# target_link_libraries(q3qcustomplottest
#     ${QCUSTOM_PLOT_LIBS_DIR}/${QCUSTOM_PLOT_LIB}
#     m)
