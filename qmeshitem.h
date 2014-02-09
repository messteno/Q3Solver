#ifndef QMESHITEM_H
#define QMESHITEM_H

#include <QPainter>

class QMeshItem
{
public:
    QMeshItem();
    virtual void draw(QPainter &painter, qreal scaleX, qreal scaleY) = 0;
};

#endif // QMESHITEM_H
