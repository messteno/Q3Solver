include_directories(${3RDPARTY_HEADERS_DIR} ${ANI_HEADERS_DIR} ${CMAKE_SOURCE_DIR}/source/q3plot ${CMAKE_SOURCE_DIR}/source/q3mesh ${CMAKE_SOURCE_DIR}/source/q3ani2d ${CMAKE_SOURCE_DIR}/source/q3interpolation ${CMAKE_SOURCE_DIR}/source/q3sceleton ${CMAKE_SOURCE_DIR}/source/q3director ${CMAKE_SOURCE_DIR}/source/q3boundary ${CMAKE_SOURCE_DIR}/source/q3calc ${CMAKE_SOURCE_DIR}/source/q3graph)

aux_source_directory(. q3main_SRC)
FILE(GLOB q3main_UI_list "*.ui")

qt5_wrap_ui(q3main_UI
    ${q3main_UI_list})

add_library(q3main STATIC
    ${q3main_SRC}
    ${q3main_UI})

qt5_use_modules(q3main Widgets)
target_link_libraries(q3main
    q3plot
    q3sceleton
    q3boundary
    q3mesh
    q3ani2d
    q3interpolation
    q3director
    q3calc
    q3graph
    m)

