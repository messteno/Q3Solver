#include "additemdirector.h"
#include "ui_lineadditemwidget.h"
#include "lineadditemwidget.h"
#include "qmeshitemline.h"
#include "qmeshitempoint.h"

LineAddItemWidget::LineAddItemWidget(AddItemDirector *director, const QString &addButtonText) :
    AddItemWidget(director, addButtonText),
    ui(new Ui::LineAddItemWidget)
{
    ui->setupUi(this);
    connect(ui->pushButton, SIGNAL(clicked()), this, SLOT(selected()));
    expanded_ = true;
    aPoint_ = NULL;
    bPoint_ = NULL;
}

QMeshItem* LineAddItemWidget::getItem()
{
    if (expanded_ == false)
        return NULL;
    if (!aPoint_ || !bPoint_)
        return NULL;

    return new QMeshItemLine(aPoint_, bPoint_);
}

void LineAddItemWidget::meshPlotClicked(QMeshPlot *meshPlot)
{
    QMeshItemPoint *point = meshPlot->getClickedScenePoint();
    if (!point)
        return;

    if (!aPoint_)
    {
        aPoint_ = point;
        ui->x1Edit->setText(QString::number(point->x()));
        ui->y1Edit->setText(QString::number(point->y()));
        return;
    }

    if (!bPoint_)
    {
        bPoint_ = point;
        ui->x2Edit->setText(QString::number(point->x()));
        ui->y2Edit->setText(QString::number(point->y()));
        return;
    }
}

void LineAddItemWidget::clear()
{
    aPoint_ = NULL;
    bPoint_ = NULL;
    ui->x1Edit->clear();
    ui->y1Edit->clear();
    ui->x2Edit->clear();
    ui->y2Edit->clear();
}
