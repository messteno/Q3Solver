#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "qmeshplot.h"

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    ui->mainToolBar->hide();
    setStatusBar(0);
    meshPlot = new QMeshPlot (this);
    setCentralWidget(meshPlot);
}

MainWindow::~MainWindow()
{
    delete ui;
}
