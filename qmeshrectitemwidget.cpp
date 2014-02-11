#include <QFormLayout>
#include <QLabel>
#include <QLineEdit>

#include "qmeshrectitemwidget.h"

QMeshRectItemWidget::QMeshRectItemWidget(QWidget *parent) :
    QMeshItemWidget(parent)
{
    QFormLayout *layout = new QFormLayout(this);
    QLabel *xLabel = new QLabel("x");
    QLineEdit *xEdit = new QLineEdit();
    layout->addRow(xLabel, xEdit);
    QLabel *yLabel = new QLabel("y");
    QLineEdit *yEdit = new QLineEdit();
    layout->addRow(yLabel, yEdit);
    QLabel *widthLabel = new QLabel("width");
    QLineEdit *widthEdit = new QLineEdit();
    layout->addRow(widthLabel, widthEdit);
    QLabel *heightLabel = new QLabel("height");
    QLineEdit *heightEdit = new QLineEdit();
    layout->addRow(heightLabel, heightEdit);

    widgetName_ = "Добавить прямоугольник";
    addButton_ = new QPushButton(widgetName_, this);
}
