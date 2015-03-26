include_directories(${ANI_HEADERS_DIR} ${CMAKE_SOURCE_DIR}/source/q3plot ${CMAKE_SOURCE_DIR}/source/q3mesh ${CMAKE_SOURCE_DIR}/source/q3ani2d ${CMAKE_SOURCE_DIR}/source/q3sceleton)

aux_source_directory(. q3mesh_builder_SRC)
FILE(GLOB q3mesh_builder_UI_list "*.ui")

qt5_wrap_ui(q3mesh_builder_UI
    ${q3mesh_builder_UI_list})

add_library(q3mesh_builder STATIC
    ${q3mesh_builder_SRC}
    ${q3mesh_builder_UI})

qt5_use_modules(q3mesh_builder Widgets)
target_link_libraries(q3mesh_builder
    q3mesh
    q3plot
    q3ani2d
    q3sceleton
    m)

