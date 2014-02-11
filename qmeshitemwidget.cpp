#include <QLayout>
#include <QFormLayout>
#include <QLabel>
#include <QLineEdit>
#include "qmeshitemwidget.h"

QMeshItemWidget::QMeshItemWidget(QWidget *parent) :
    QWidget(parent)
{
}

QMeshItemWidget::~QMeshItemWidget()
{
}

void QMeshItemWidget::showWidget()
{
    this->show();
}
