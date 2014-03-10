#ifndef QMESHITEMLINE_H
#define QMESHITEMLINE_H

#include "qmeshitem.h"
#include "qmeshitempoint.h"

class QMeshItemLine : public QMeshItem
{
public:
    explicit QMeshItemLine(QMeshItemPoint *a, QMeshItemPoint *b);
    virtual void draw(QPainter &painter, qreal scaleX, qreal scaleY) const;
private:
    QMeshItemPoint *a_;
    QMeshItemPoint *b_;
};

#endif // QMESHITEMLINE_H
