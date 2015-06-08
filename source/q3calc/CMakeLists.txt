# process input parameters

set(CMAKE_BUILD_TYPE Release)

include_directories(${CMAKE_SOURCE_DIR}/source/q3plot)
include_directories(${CMAKE_SOURCE_DIR}/source/q3sceleton)
include_directories(${CMAKE_SOURCE_DIR}/source/q3boundary)
include_directories(${CMAKE_SOURCE_DIR}/source/q3mesh)

aux_source_directory(. q3calc_SRC)
FILE(GLOB q3calc_UI_list "*.ui")

qt5_wrap_ui(q3calc_UI
    ${q3calc_UI_list})

add_library(q3calc STATIC
    ${q3calc_SRC}
    ${q3calc_UI})

qt5_use_modules(q3calc Widgets)
target_link_libraries(q3calc optimized
    q3plot
    q3sceleton
    q3boundary
    q3mesh
    m)
