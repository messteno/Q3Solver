add_subdirectory(q3mesh_builder)
add_subdirectory(q3test)

include_directories(${ANI_HEADERS_DIR} q3mesh_builder)
link_directories(${ANI_LIBS_DIR})

aux_source_directory(. q3solver_SRC)
FILE(GLOB q3solver_UI_list "*.ui")

qt5_wrap_ui(q3solver_UI
    ${q3solver_UI_list})

add_executable(q3solver
    ${q3solver_SRC}
    ${q3solver_UI})

qt5_use_modules(q3solver Widgets)
target_link_libraries(
    q3solver
    q3mesh_builder)

