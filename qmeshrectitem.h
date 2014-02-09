#ifndef QMRECTITEM_H
#define QMRECTITEM_H

#include <QPainter>
#include "qmeshitem.h"

class QMeshRectItem : public QMeshItem
{
public:
    explicit QMeshRectItem();
    explicit QMeshRectItem(QRectF rect);
    void draw(QPainter &painter, qreal scaleX, qreal scaleY);

private:
    QRectF rect_;
};

#endif // QMRECTITEM_H
