#include "q3calculuseditor.h"
#include "q3movedirector.h"
#include "ui_q3calculuseditor.h"

Q3CalculusEditor::Q3CalculusEditor(Q3Plot *plot, Q3Mesh *mesh, QWidget *parent) :
    QWidget(parent),
    mesh_(mesh),
    ui(new Ui::Q3CalculusEditor)
{
    ui->setupUi(this);

    directorManager_ = new Q3DirectorManager(this);
    Q3Director *moveDirector = new Q3MoveDirector(this);
    directorManager_->addDirector(moveDirector);
    directorManager_->setPlot(plot);

    // TODO:
    qreal tau = 0.001;
    qreal Re = 100;
    calc_ = new Q3Calc(mesh, tau, Re, this);

    connect(calc_, SIGNAL(updateInfo()), this, SLOT(updateInfo()));
}

Q3CalculusEditor::~Q3CalculusEditor()
{
    delete ui;
}

void Q3CalculusEditor::on_startCalculusButton_clicked()
{
    calc_->start();
}

void Q3CalculusEditor::on_stopalculusButton_clicked()
{
    calc_->abort();
}

void Q3CalculusEditor::updateInfo()
{
    ui->calcInfoLabel->setText(calc_->info());
}
