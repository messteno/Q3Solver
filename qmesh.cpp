#include "qmesh.h"
#include "ui_qmesh.h"
#include <QDebug>
#include <QList>

QMesh::QMesh(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::QMesh)
{
    ui->setupUi(this);
    addItemDirector_ = new AddItemDirector(ui->meshPlot, this);
    ui->addWidgetsLayout->addWidget(addItemDirector_);
}

QMesh::~QMesh()
{
    delete addItemDirector_;
}

void QMesh::on_addElementButton_clicked()
{
    addItemDirector_->show();
}

void QMesh::on_cancelElementButton_clicked()
{
    addItemDirector_->hide();
}
