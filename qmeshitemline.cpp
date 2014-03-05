#include "qmeshitemline.h"

QMeshItemLine::QMeshItemLine(qreal x1, qreal y1, qreal x2, qreal y2)
{
    x1_ = x1;
    y1_ = y1;
    x2_ = x2;
    y2_ = y2;
}

void QMeshItemLine::draw(QPainter &painter, qreal scaleX, qreal scaleY)
{
    painter.drawLine(x1_ * scaleX, y1_ * scaleY, x2_ * scaleX, y2_ * scaleY);
}
