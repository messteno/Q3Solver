include_directories(${CMAKE_SOURCE_DIR}/source/q3plot)

aux_source_directory(. q3sceleton_SRC)
FILE(GLOB q3sceleton_UI_list "*.ui")

qt5_wrap_ui(q3sceleton_UI
    ${q3sceleton_UI_list})

add_library(q3sceleton STATIC
    ${q3sceleton_SRC}
    ${q3sceleton_UI})

qt5_use_modules(q3sceleton Widgets)
target_link_libraries(q3sceleton
    q3plot
    m)

