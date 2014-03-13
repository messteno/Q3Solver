#include "additemdirector.h"
#include "ui_segmentadditemwidget.h"
#include "segmentadditemwidget.h"
#include "qmeshitemline.h"
#include "qmeshitempoint.h"

SegmentAddItemWidget::SegmentAddItemWidget(AddItemDirector *director, const QString &addButtonText) :
    AddItemWidget(director, addButtonText),
    ui(new Ui::LineAddItemWidget)
{
    ui->setupUi(this);
    connect(ui->pushButton, SIGNAL(clicked()), this, SLOT(selected()));
    expanded_ = true;
    aPoint_ = NULL;
    bPoint_ = NULL;
}

bool SegmentAddItemWidget::addItem()
{
    if (expanded_ == false)
        return false;
    if (!aPoint_ || !bPoint_)
        return false;

    director_->addItem(new QMeshItemLine(aPoint_, bPoint_));
    return true;
}

void SegmentAddItemWidget::meshPlotClicked(QMeshPlot *meshPlot)
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

void SegmentAddItemWidget::clear()
{
    aPoint_ = NULL;
    bPoint_ = NULL;
    ui->x1Edit->clear();
    ui->y1Edit->clear();
    ui->x2Edit->clear();
    ui->y2Edit->clear();
}
