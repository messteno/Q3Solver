#ifndef QMESHITEM_H
#define QMESHITEM_H

#include <QPainter>

class QMeshItem
{
public:
    QMeshItem();
    virtual ~QMeshItem();
    virtual void draw(QPainter &painter, qreal scaleX, qreal scaleY) const = 0;
    virtual QString getName();
    virtual QString getValueText();
    virtual QString getTypeText();
};

#endif // QMESHITEM_H
