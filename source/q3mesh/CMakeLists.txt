set(CMAKE_BUILD_TYPE Release)

include_directories(
    ${3RDPARTY_HEADERS_DIR}
    ${CMAKE_SOURCE_DIR}/source/q3plot
    ${CMAKE_SOURCE_DIR}/source/q3sceleton
    ${CMAKE_SOURCE_DIR}/source/q3boundary
    ${CMAKE_SOURCE_DIR}/source/q3algorithms
)

link_directories(${3RDPARTY_LIBS_DIR})

aux_source_directory(. q3mesh_SRC)
FILE(GLOB q3mesh_UI_list "*.ui")

qt5_wrap_ui(q3mesh_UI
    ${q3mesh_UI_list})

add_library(q3mesh STATIC
    ${q3mesh_SRC}
    ${q3mesh_UI})

qt5_use_modules(q3mesh Widgets)
target_link_libraries(q3mesh
    q3plot
    q3sceleton
    q3boundary
    q3algorithms
    m)

