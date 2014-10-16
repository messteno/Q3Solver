#ifndef QMESHITEMLINE_H
#define QMESHITEMLINE_H

#include "qmeshitem.h"
#include "qmeshitempoint.h"

class QMeshItemLine : public QMeshItem
{
public:
    explicit QMeshItemLine(QMeshItemPoint *a, QMeshItemPoint *b);
    virtual void draw(QPainter &painter, qreal scaleX, qreal scaleY) const;
    virtual QString getName();
    virtual QString getValueText();
    QMeshItemPoint* a();
    QMeshItemPoint* b();

private:
    QMeshItemPoint *a_;
    QMeshItemPoint *b_;
};

#endif // QMESHITEMLINE_H
