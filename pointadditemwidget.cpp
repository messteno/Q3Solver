#include "additemdirector.h"
#include "ui_pointadditemwidget.h"
#include "pointadditemwidget.h"
#include "qmeshitempoint.h"

PointAddItemWidget::PointAddItemWidget(AddItemDirector *director, const QString &addButtonText) :
    AddItemWidget(director, addButtonText),
    ui(new Ui::PointAddItemWidget)
{
    ui->setupUi(this);
    connect(ui->pushButton, SIGNAL(clicked()), this, SLOT(selected()));
    expanded_ = true;
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
