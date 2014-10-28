# process input parameters
if (ENABLE_SYSTEM_LAPACK)
  set(LAPACK_LIBS "lapack;blas")
else ()
  set(LAPACK_LIBS "lapack-3.2;lapack_ext-3.2;blas-3.2")
endif()

include_directories(${ANI_HEADERS_DIR})
link_directories(${ANI_LIBS_DIR})

set(qtrisolver_SRC
    additemdirector.cpp
    additemdirectorstate.cpp
    additemdirectorstateitemadd.cpp
    additemdirectorstateitemselect.cpp
    additemwidget.cpp
    ani2dmeshgenerator.cpp
    itemlistmodel.cpp
    main.cpp
    mainwindow.cpp
    mesh.cpp
    meshgenerator.cpp
    pointadditemwidget.cpp
    qani2d.cpp
    qmesh.cpp
    qmeshitem.cpp
    qmeshitemline.cpp
    qmeshitempoint.cpp
    qmeshplot.cpp
    segmentadditemwidget.cpp)

qt5_wrap_ui(qtrisolver_UI
    mainwindow.ui
    pointadditemwidget.ui
    qmesh.ui
    segmentadditemwidget.ui)

add_executable(qtrisolver
    ${qtrisolver_SRC}
    ${qtrisolver_UI})

qt5_use_modules(qtrisolver Widgets)
add_dependencies(qtrisolver ani2D)
target_link_libraries(qtrisolver
    aft2D-${ANI2D_VERSION}
    c2f2D-${ANI2D_VERSION}
    view2D-${ANI2D_VERSION}
    mba2D-${ANI2D_VERSION}
    ${LAPACK_LIBS}
    gfortran
    m)

