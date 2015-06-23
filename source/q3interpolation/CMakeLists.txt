include_directories(
    ${3RDPARTY_HEADERS_DIR}
    ${CMAKE_SOURCE_DIR}/source/q3mesh
    ${CMAKE_SOURCE_DIR}/source/q3sceleton
    ${CMAKE_SOURCE_DIR}/source/q3plot
    ${CMAKE_SOURCE_DIR}/source/q3boundary
)

link_directories(${3RDPARTY_LIBS_DIR})

aux_source_directory(. q3interpolation_SRC)
FILE(GLOB q3interpolation_UI_list "*.ui")

qt5_wrap_ui(q3interpolation_UI
    ${q3interpolation_UI_list})

add_library(q3interpolation STATIC
    ${q3interpolation_SRC}
    ${q3interpolation_UI})

qt5_use_modules(q3interpolation Widgets)
target_link_libraries(q3interpolation
    nn
    q3plot
    q3mesh
    q3sceleton
    q3boundary
    m)

