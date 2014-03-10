#ifndef QMESHITEMPOINT_H
#define QMESHITEMPOINT_H

#include "qmeshitem.h"

class QMeshItemPoint : public QMeshItem
{
public:
    static const int pointSize_;

    explicit QMeshItemPoint(qreal x, qreal y);
    virtual void draw(QPainter &painter, qreal scaleX, qreal scaleY) const;
    qreal x();
    qreal y();
private:
    qreal x_;
    qreal y_;
};

#endif // QMESHITEMPOINT_H
