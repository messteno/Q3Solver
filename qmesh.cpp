#include "qmesh.h"
#include "qmeshplot.h"
#include "ui_qmesh.h"
#include <QDebug>
#include <QList>

#include "itemlistmodel.h"

QMesh::QMesh(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::QMesh)
{
    ui->setupUi(this);

    addItemDirector_ = new AddItemDirector(this);
    meshPlot_ = new QMeshPlot(this);

    ui->addWidgetsLayout->addWidget(addItemDirector_);
    ui->plotLayout->addWidget(meshPlot_);

    connect(meshPlot_, SIGNAL(mouseClicked(QMeshPlot *)),
            addItemDirector_, SLOT(meshPlotClicked(QMeshPlot *)));

    itemListModel_ = new ItemListModel();
    ui->itemList->setModel(itemListModel_);
}

QMesh::~QMesh()
{
    delete addItemDirector_;
    delete meshPlot_;
    delete itemListModel_;

    foreach (QMeshItem *item, items_)
        delete item;
    items_.clear();
}

void QMesh::addItem(QMeshItem *item)
{
    if (item)
    {
        items_.push_back(item);
        meshPlot_->repaint();
        itemListModel_->addItem(item);
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
