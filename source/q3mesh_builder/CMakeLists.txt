# process input parameters
if (ENABLE_SYSTEM_LAPACK)
  set(LAPACK_LIBS "lapack;blas")
else ()
  set(LAPACK_LIBS "lapack-3.2;lapack_ext-3.2;blas-3.2")
endif()

include_directories(${ANI_HEADERS_DIR})
link_directories(${ANI_LIBS_DIR})

aux_source_directory(. q3mesh_builder_SRC)
FILE(GLOB q3mesh_builder_UI_list "*.ui")

qt5_wrap_ui(q3mesh_builder_UI
    ${q3mesh_builder_UI_list})

add_library(q3mesh_builder STATIC
    ${q3mesh_builder_SRC}
    ${q3mesh_builder_UI})

qt5_use_modules(q3mesh_builder Widgets)
add_dependencies(q3mesh_builder ani2D)
target_link_libraries(q3mesh_builder
    aft2D-${ANI2D_VERSION}
    c2f2D-${ANI2D_VERSION}
    view2D-${ANI2D_VERSION}
    mba2D-${ANI2D_VERSION}
    ${LAPACK_LIBS}
    gfortran
    m)

