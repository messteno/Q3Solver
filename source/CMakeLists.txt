cmake_policy(SET CMP0043 NEW)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC")
find_package(OpenMP)
if(OPENMP_FOUND)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
endif()

add_subdirectory(q3plot)
add_subdirectory(q3algorithms)
add_subdirectory(q3sceleton)
add_subdirectory(q3boundary)
add_subdirectory(q3mesh)
add_subdirectory(q3ani2d)
add_subdirectory(q3interpolation)
add_subdirectory(q3director)
add_subdirectory(q3calc)
add_subdirectory(q3graph)
add_subdirectory(q3main)
add_subdirectory(q3test)

include_directories(
    ${3RDPARTY_HEADERS_DIR}
    ${ANI_HEADERS_DIR}
    ${QCUSTOM_PLOT_HEADERS_DIR}
    q3plot
    q3algorithms
    q3sceleton
    q3boundary
    q3mesh
    q3ani2d
    q3interpolation
    q3director
    q3calc
    q3graph
    q3main
)
link_directories(${3RDPARTY_LIBS_DIR} ${ANI_LIBS_DIR} ${QCUSTOM_PLOT_LIBS_DIR})

aux_source_directory(. q3solver_SRC)
FILE(GLOB q3solver_UI_list "*.ui")

qt5_wrap_ui(q3solver_UI
    ${q3solver_UI_list})

add_executable(q3solver
    ${q3solver_SRC}
    ${q3solver_UI})

qt5_use_modules(q3solver Widgets)
target_link_libraries(q3solver
    q3plot
    q3algorithms
    q3sceleton
    q3boundary
    optimized q3mesh
    q3ani2d
    q3interpolation
    q3director
    optimized q3calc
    q3graph
    q3main)
