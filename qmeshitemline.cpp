#include "qmeshitemline.h"
#include "qmeshitempoint.h"

QMeshItemLine::QMeshItemLine(QMeshItemPoint *a, QMeshItemPoint *b)
{
    a_ = a;
    b_ = b;
}

void QMeshItemLine::draw(QPainter &painter, qreal scaleX, qreal scaleY) const
{
    if (!a_ || !b_)
        return;
    painter.drawLine(a_->x() * scaleX, a_->y() * scaleY,
                     b_->x() * scaleX, b_->y() * scaleY);
}

QString QMeshItemLine::getName()
{
    return QString("Отрезок");
}

QString QMeshItemLine::getValueText()
{
    return QString("(%1, %2) - (%3, %4)")
            .arg(a_->x(), 0, 'f', 2)
            .arg(a_->y(), 0, 'f', 2)
            .arg(b_->x(), 0, 'f', 2)
            .arg(b_->y(), 0, 'f', 2);
}
