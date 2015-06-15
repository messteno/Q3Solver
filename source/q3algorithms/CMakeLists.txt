set(CMAKE_BUILD_TYPE Release)

aux_source_directory(. q3algorithms_SRC)
FILE(GLOB q3algorithms_UI_list "*.ui")

qt5_wrap_ui(q3algorithms_UI
    ${q3algorithms_UI_list})

add_library(q3algorithms STATIC
    ${q3algorithms_SRC}
    ${q3algorithms_UI})

qt5_use_modules(q3algorithms Widgets)
target_link_libraries(q3algorithms
    m)
