#include <QFormLayout>
#include <QLabel>
#include <QLineEdit>

#include "qmeshpointitemwidget.h"

QMeshPointItemWidget::QMeshPointItemWidget(QWidget *parent) :
    QMeshItemWidget(parent)
{
    QFormLayout *layout = new QFormLayout(this);
    QLabel *xLabel = new QLabel("x");
    QLineEdit *xEdit = new QLineEdit();
    layout->addRow(xLabel, xEdit);
    QLabel *yLabel = new QLabel("y");
    QLineEdit *yEdit = new QLineEdit();
    layout->addRow(yLabel, yEdit);

    widgetName_ = "Добавить точку";
    addButton_ = new QPushButton(widgetName_, this);
}
