aux_source_directory(. q3plot_SRC)
FILE(GLOB q3plot_UI_list "*.ui")

qt5_wrap_ui(q3plot_UI
    ${q3plot_UI_list})

add_library(q3plot STATIC
    ${q3plot_SRC}
    ${q3plot_UI})

qt5_use_modules(q3plot Widgets)
target_link_libraries(q3plot
    m)

