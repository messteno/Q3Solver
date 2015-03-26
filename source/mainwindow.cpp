#include "mainwindow.h"
#include "ui_mainwindow.h"

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    solver_ = new Q3Solver(this);
    setCentralWidget(solver_);
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::on_actionMeshBuilder_triggered()
{
    Q3MeshBuilder* meshBuilder = new Q3MeshBuilder(solver_->mesh(),
                                                   solver_->sceleton(), this);
    meshBuilder->exec();
    delete meshBuilder;
}
