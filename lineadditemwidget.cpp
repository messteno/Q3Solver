#include "additemdirector.h"
#include "ui_lineadditemwidget.h"
#include "lineadditemwidget.h"
#include "qmeshitemline.h"

LineAddItemWidget::LineAddItemWidget(AddItemDirector *director, const QString &addButtonText) :
    AddItemWidget(director, addButtonText),
    ui(new Ui::LineAddItemWidget)
{
    ui->setupUi(this);
    connect(ui->pushButton, SIGNAL(clicked()), this, SLOT(selected()));
    expanded_ = true;
}

QMeshItem* LineAddItemWidget::getItem()
{
    if (expanded_ == false)
        return NULL;
    if (ui->x1Edit->text().isEmpty() || ui->y1Edit->text().isEmpty() ||
        ui->x2Edit->text().isEmpty() || ui->y2Edit->text().isEmpty())
    {
        return NULL;
    }

    bool ok;
    ok = true;
    qreal x1 = ui->x1Edit->text().toDouble(&ok);
    if (!ok)
        return NULL;
    ok = true;
    qreal y1 = ui->y1Edit->text().toDouble(&ok);
    if (!ok)
        return NULL;
    ok = true;
    qreal x2 = ui->x2Edit->text().toDouble(&ok);
    if (!ok)
        return NULL;
    ok = true;
    qreal y2 = ui->y2Edit->text().toDouble(&ok);
    if (!ok)
        return NULL;
    return new QMeshItemLine(x1, y1, x2, y2);
}
