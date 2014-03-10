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

qreal QMeshItemPoint::x()
{
    return x_;
}

qreal QMeshItemPoint::y()
{
    return y_;
}
