#ifndef QMESHITEMLINE_H
#define QMESHITEMLINE_H

#include "qmeshitem.h"

class QMeshItemLine : public QMeshItem
{
public:
    explicit QMeshItemLine(qreal x1, qreal y1, qreal x2, qreal y2);
    virtual void draw(QPainter &painter, qreal scaleX, qreal scaleY);
private:
    qreal x1_;
    qreal y1_;
    qreal x2_;
    qreal y2_;
};

#endif // QMESHITEMLINE_H
