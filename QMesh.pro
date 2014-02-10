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
    qmeshrectitem.cpp \
    qmeshplot.cpp

HEADERS  += mainwindow.h \
    qmeshitem.h \
    qmeshrectitem.h \
    qmeshplot.h

FORMS    += mainwindow.ui
