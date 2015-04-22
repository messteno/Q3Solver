# process input parameters
include_directories(${CMAKE_SOURCE_DIR}/source/q3plot ${CMAKE_SOURCE_DIR}/source/q3sceleton ${CMAKE_SOURCE_DIR}/source/q3boundary ${CMAKE_SOURCE_DIR}/source/q3mesh)

aux_source_directory(. q3calc_SRC)
FILE(GLOB q3calc_UI_list "*.ui")

qt5_wrap_ui(q3calc_UI
    ${q3calc_UI_list})

add_library(q3calc STATIC
    ${q3calc_SRC}
    ${q3calc_UI})

qt5_use_modules(q3calc Widgets)
target_link_libraries(q3calc
    q3plot
    q3sceleton
    q3boundary
    q3mesh
    m)

