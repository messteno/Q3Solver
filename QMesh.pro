#-------------------------------------------------
#
# Project created by QtCreator 2014-02-09T17:13:58
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = QMesh
TEMPLATE = app

LIBS += -L"$$_PRO_FILE_PWD_/../ani2D-3.0/lib/" \
    -lc2f2D-3.0 \
    -lview2D-3.0 \
    -lmba2D-3.0 \
    -llapack-3.2 \
    -lblas-3.2 \
    -llapack_ext-3.2 \
    -laft2D-3.0 \
    -lgfortran

INCLUDEPATH += "$$_PRO_FILE_PWD_/../ani2D-3.0/include/"

SOURCES += main.cpp\
        mainwindow.cpp \
    qmeshitem.cpp \
    qmeshplot.cpp \
    qmesh.cpp \
    additemdirector.cpp \
    additemwidget.cpp \
    pointadditemwidget.cpp \
    additemdirectorstate.cpp \
    additemdirectorstateitemselect.cpp \
    additemdirectorstateitemadd.cpp \
    qmeshitempoint.cpp \
    qmeshitemline.cpp \
    itemlistmodel.cpp \
    meshgenerator.cpp \
    mesh.cpp \
    ani2dmeshgenerator.cpp \
    qani2d.cpp \
    segmentadditemwidget.cpp

HEADERS  += mainwindow.h \
    qmeshitem.h \
    qmeshplot.h \
    qmesh.h \
    additemwidget.h \
    additemdirector.h \
    pointadditemwidget.h \
    additemdirectorstate.h \
    additemdirectorstateitemselect.h \
    additemdirectorstateitemadd.h \
    qmeshitempoint.h \
    qmeshitemline.h \
    itemlistmodel.h \
    meshgenerator.h \
    mesh.h \
    ani2dmeshgenerator.h \
    qani2d.h \
    segmentadditemwidget.h

FORMS    += mainwindow.ui \
    qmesh.ui \
    pointadditemwidget.ui \
    segmentadditemwidget.ui


