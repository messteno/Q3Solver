#include "additemdirector.h"
#include "ui_pointadditemwidget.h"
#include "pointadditemwidget.h"
#include "qmeshitempoint.h"

PointAddItemWidget::PointAddItemWidget(AddItemDirector *director, QWidget *parent) :
    AddItemWidget(director, parent),
    ui(new Ui::PointAddItemWidget)
{
    ui->setupUi(this);
    connect(ui->pushButton, SIGNAL(clicked()), this, SLOT(selected()));
    expanded_ = true;
}

void PointAddItemWidget::expand()
{
    ui->xLabel->show();
    ui->yLabel->show();
    ui->xEdit->show();
    ui->yEdit->show();
    expanded_ = true;
}

void PointAddItemWidget::shrink()
{
    ui->xLabel->hide();
    ui->yLabel->hide();
    ui->xEdit->hide();
    ui->yEdit->hide();
    expanded_ = false;
}

QMeshItem* PointAddItemWidget::getItem()
{
    if (expanded_ == false)
        return NULL;
    if (ui->xEdit->text().isEmpty() || ui->yEdit->text().isEmpty())
        return NULL;
    bool ok = true;
    qreal x = ui->xEdit->text().toDouble(&ok);
    if (!ok)
        return NULL;
    ok = true;
    qreal y = ui->yEdit->text().toDouble(&ok);
    if (!ok)
        return NULL;
    return new QMeshItemPoint(x, y);
}
