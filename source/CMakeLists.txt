add_subdirectory(q3mesh_builder)
include_directories(${ANI_HEADERS_DIR} q3mesh_builder)
link_directories(${ANI_LIBS_DIR})

aux_source_directory(. qtrisolver_SRC)
FILE(GLOB qtrisolver_UI_list "*.ui")

qt5_wrap_ui(qtrisolver_UI
    ${qtrisolver_UI_list})

add_executable(qtrisolver
    ${qtrisolver_SRC}
    ${qtrisolver_UI})

qt5_use_modules(qtrisolver Widgets)
target_link_libraries(qtrisolver
    q3mesh_builder)

