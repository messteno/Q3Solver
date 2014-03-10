#include "qmesh.h"
#include "ui_qmesh.h"
#include <QDebug>
#include <QList>

QMesh::QMesh(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::QMesh)
{
    ui->setupUi(this);
    addItemDirector_ = new AddItemDirector(this);
    ui->addWidgetsLayout->addWidget(addItemDirector_);
    connect(ui->meshPlot, SIGNAL(mouseClicked(QMeshPlot *)),
            addItemDirector_, SLOT(meshPlotClicked(QMeshPlot *)));
}

QMesh::~QMesh()
{
    delete addItemDirector_;

    foreach (QMeshItem *item, items_)
        delete item;
    items_.clear();
}

void QMesh::addItem(QMeshItem *item)
{
    if (item)
    {
        items_.push_back(item);
        ui->meshPlot->repaint();
    }
}

const QList<QMeshItem *>& QMesh::getItems()
{
    return items_;
}

void QMesh::on_addElementButton_clicked()
{
    addItemDirector_->show();
}

void QMesh::on_cancelElementButton_clicked()
{
    addItemDirector_->hide();
}
