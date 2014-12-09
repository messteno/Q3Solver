#include "mainwindow.h"
#include "ui_mainwindow.h"

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    setStatusBar(0);
    meshBuilder = new Q3MeshBuilder(this);
    setCentralWidget(meshBuilder);
}

MainWindow::~MainWindow()
{
    delete ui;
}
