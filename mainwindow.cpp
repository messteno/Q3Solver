#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "qmesh.h"

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    setStatusBar(0);
    mesh = new QMesh (this);
    setCentralWidget(mesh);
}

MainWindow::~MainWindow()
{
    delete ui;
}
