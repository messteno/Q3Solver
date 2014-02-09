#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "qmesh.h"

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    mesh = new QMesh (this);
    setCentralWidget(mesh);
}

MainWindow::~MainWindow()
{
    delete ui;
}
