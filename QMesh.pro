#-------------------------------------------------
#
# Project created by QtCreator 2014-02-09T17:13:58
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = QMesh
TEMPLATE = app


SOURCES += main.cpp\
        mainwindow.cpp \
    qmeshitem.cpp \
    qmeshplot.cpp \
    qmesh.cpp \
    additemdirector.cpp \
    additemwidget.cpp \
    pointadditemwidget.cpp \
    lineadditemwidget.cpp \
    additemdirectorstate.cpp \
    additemdirectorstateitemselect.cpp \
    additemdirectorstateitemadd.cpp \
    qmeshitempoint.cpp

HEADERS  += mainwindow.h \
    qmeshitem.h \
    qmeshplot.h \
    qmesh.h \
    additemdirector.h \
    additemwidget.h \
    pointadditemwidget.h \
    lineadditemwidget.h \
    additemdirectorstate.h \
    additemdirectorstateitemselect.h \
    additemdirectorstateitemadd.h \
    qmeshitempoint.h

FORMS    += mainwindow.ui \
    qmesh.ui \
    pointadditemwidget.ui \
    lineadditemwidget.ui
