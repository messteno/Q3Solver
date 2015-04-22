# process input parameters
include_directories(${CMAKE_SOURCE_DIR}/source/q3plot ${CMAKE_SOURCE_DIR}/source/q3sceleton)

aux_source_directory(. q3director_SRC)
FILE(GLOB q3director_UI_list "*.ui")

qt5_wrap_ui(q3director_UI
    ${q3director_UI_list})

add_library(q3director STATIC
    ${q3director_SRC}
    ${q3director_UI})

qt5_use_modules(q3director Widgets)
target_link_libraries(q3director
    q3plot
    q3sceleton
    m)

