#include "q3solver.h"
#include "q3movedirector.h"
#include "ui_q3solver.h"

Q3Solver::Q3Solver(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::Q3Solver)
{
    ui->setupUi(this);
    directorManager_ = new Q3DirectorManager(this);

    Q3Director *moveDirector = new Q3MoveDirector(this);
    directorManager_->addDirector(moveDirector);

    directorManager_->setPlot(ui->plotWidget);

    mesh_ = new Q3Mesh(this);
    ui->plotWidget->addDrawable(static_cast<Q3PlotDrawable *>(mesh_));

    sceleton_ = new Q3Sceleton(this);
}

Q3Solver::~Q3Solver()
{
    delete ui;
}

void Q3Solver::paintEvent(QPaintEvent *event)
{
    ui->meshInfoLabel->setText(mesh_->info());
}

Q3Mesh* Q3Solver::mesh() const
{
    return mesh_;
}

Q3Sceleton *Q3Solver::sceleton() const
{
    return sceleton_;
}
