#include "qmesh.h"
#include "qmeshrectitemwidget.h"
#include "qmeshpointitemwidget.h"
#include "ui_qmesh.h"
#include <QDebug>
#include <QList>

QMesh::QMesh(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::QMesh)
{
    ui->setupUi(this);
    setState(QMesh::stateNone);

    itemWidgetlist_.append(new QMeshPointItemWidget());
    itemWidgetlist_.append(new QMeshRectItemWidget());
    foreach(QMeshItemWidget *widget, itemWidgetlist_)
    {
        ui->addWidgetsLayout->addWidget(widget);
        widget->hide();
    }
}

QMesh::~QMesh()
{
    foreach (QMeshItemWidget* widget, itemWidgetlist_)
        delete widget;
    itemWidgetlist_.clear();
    delete ui;
}

void QMesh::setState(QMesh::qmeshState state)
{
    state_ = state;
    switch (state_)
    {
    case stateElementAdding:
        clearWidgetsLayout();
        break;
    case stateElementSelection:
        ui->addElementButton->setEnabled(false);
        ui->cancelElementButton->setEnabled(true);
        addElementButtons();
        break;
    case stateNone:
    default:
        clearWidgetsLayout();
        ui->addElementButton->setEnabled(true);
        ui->cancelElementButton->setEnabled(false);
        break;
    }
}

void QMesh::on_addElementButton_clicked()
{
    setState(stateElementSelection);
}

void QMesh::on_cancelElementButton_clicked()
{
    setState(stateNone);
}

void QMesh::on_widgetElementButton_clicked()
{
    setState(stateElementAdding);
}

void QMesh::addElementButtons()
{
    foreach(QMeshItemWidget *widget, itemWidgetlist_)
    {
        ui->addWidgetsLayout->addWidget(widget->addButton_);
        widget->addButton_->show();
        connect (widget->addButton_, SIGNAL(clicked()), this, SLOT(on_widgetElementButton_clicked()));
        connect (widget->addButton_, SIGNAL(clicked()), widget, SLOT(showWidget()));
    }
}

void QMesh::clearWidgetsLayout()
{
    foreach(QMeshItemWidget *widget, itemWidgetlist_)
    {
        widget->addButton_->hide();
        widget->hide();
    }
}
