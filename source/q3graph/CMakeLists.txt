include_directories(${3RDPARTY_HEADERS_DIR} ${ANI_HEADERS_DIR} ${CMAKE_SOURCE_DIR}/source/q3plot ${CMAKE_SOURCE_DIR}/source/q3mesh ${CMAKE_SOURCE_DIR}/source/q3ani2d ${CMAKE_SOURCE_DIR}/source/q3interpolation ${CMAKE_SOURCE_DIR}/source/q3sceleton ${CMAKE_SOURCE_DIR}/source/q3director ${CMAKE_SOURCE_DIR}/source/q3boundary ${CMAKE_SOURCE_DIR}/source/q3calc)

aux_source_directory(. q3graph_SRC)
FILE(GLOB q3graph_UI_list "*.ui")

qt5_wrap_ui(q3graph_UI
    ${q3graph_UI_list})

add_library(q3graph STATIC
    ${q3graph_SRC}
    ${q3graph_UI})

qt5_use_modules(q3graph Widgets)
target_link_libraries(q3graph
    q3plot
    q3sceleton
    q3boundary
    q3mesh
    q3ani2d
    q3interpolation
    q3director
    q3calc
    m)

