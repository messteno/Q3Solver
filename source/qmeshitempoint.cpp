#include <QString>
#include "qmeshitempoint.h"

const int QMeshItemPoint::pointSize_ = 5;

QMeshItemPoint::QMeshItemPoint(qreal x, qreal y)
{
    x_ = x;
    y_ = y;
}

void QMeshItemPoint::draw(QPainter &painter, qreal scaleX, qreal scaleY) const
{
    QPointF point = QPointF(x_ * scaleX, y_ * scaleY);
    painter.drawEllipse(point, pointSize_, pointSize_);
}

QString QMeshItemPoint::getName()
{
    return QString("Точка");
}

QString QMeshItemPoint::getValueText()
{
    return QString("(%1, %2)")
            .arg(x_, 0, 'f', 2)
            .arg(y_, 0, 'f', 2);
}

qreal QMeshItemPoint::x()
{
    return x_;
}

qreal QMeshItemPoint::y()
{
    return y_;
}
