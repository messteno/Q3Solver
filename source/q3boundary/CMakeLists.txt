# process input parameters
include_directories(
    ${3RDPARTY_HEADERS_DIR}
    ${CMAKE_SOURCE_DIR}/source/q3sceleton
    ${CMAKE_SOURCE_DIR}/source/q3plot
)

aux_source_directory(. q3boundary_SRC)
FILE(GLOB q3boundary_UI_list "*.ui")

qt5_wrap_ui(q3boundary_UI
    ${q3boundary_UI_list})

add_library(q3boundary STATIC
    ${q3boundary_SRC}
    ${q3boundary_UI})

qt5_use_modules(q3boundary Widgets)
target_link_libraries(q3boundary
    q3sceleton
    q3plot
    m)

if(PCHSupport_FOUND)
    MESSAGE("PCH ok")
    ADD_PRECOMPILED_HEADER (q3boundary ${3RDPARTY_HEADERS_DIR}/exprtk.h)
endif(PCHSupport_FOUND)

