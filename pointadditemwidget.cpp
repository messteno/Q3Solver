#include "additemdirector.h"
#include "ui_pointadditemwidget.h"
#include "pointadditemwidget.h"
#include "qmeshitempoint.h"

#include <QTextStream>

PointAddItemWidget::PointAddItemWidget(AddItemDirector *director, const QString &addButtonText) :
    AddItemWidget(director, addButtonText),
    ui(new Ui::PointAddItemWidget)
{
    ui->setupUi(this);
    connect(ui->pushButton, SIGNAL(clicked()), this, SLOT(selected()));
    expanded_ = true;
}

bool PointAddItemWidget::addItem()
{
    if (expanded_ == false)
        return false;
    if (ui->xEdit->text().isEmpty() || ui->yEdit->text().isEmpty())
        return false;
    bool ok = true;
    qreal x = ui->xEdit->text().toDouble(&ok);
    if (!ok)
        return false;
    ok = true;
    qreal y = ui->yEdit->text().toDouble(&ok);
    if (!ok)
        return false;
    director_->addItem(new QMeshItemPoint(x, y));
    return true;
}

void PointAddItemWidget::meshPlotClicked(QMeshPlot *meshPlot)
{
    QPointF pos = meshPlot->getClickedScenePosition(ui->snapToGrid->isChecked());
    ui->xEdit->setText(QString::number(pos.x()));
    ui->yEdit->setText(QString::number(pos.y()));
}

void PointAddItemWidget::clear()
{
    ui->xEdit->clear();
    ui->yEdit->clear();
}
