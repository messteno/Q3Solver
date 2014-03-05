#include "qmeshitempoint.h"

QMeshItemPoint::QMeshItemPoint(qreal x, qreal y)
{
    x_ = x;
    y_ = y;
}

void QMeshItemPoint::draw(QPainter &painter, qreal scaleX, qreal scaleY)
{
    QPointF point = QPointF(x_ * scaleX, y_ * scaleY);
    painter.drawEllipse(point, 5, 5);
}
