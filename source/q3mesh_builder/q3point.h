#ifndef Q3POINT_H
#define Q3POINT_H

#include "q3sceletonitem.h"
#include "q3painter.h"

class Q3Point : public Q3SceletonItem
{
private:
    QPointF point_;

public:
    static const int PointSize;

    Q3Point();
    Q3Point(const Q3Point &q3point);
    Q3Point(const QPointF &point);
    virtual ~Q3Point();
    virtual void draw(Q3Painter &painter);
    virtual void move(const QPointF diff);
    virtual qreal distanceTo(const QPointF &pos) const;
    virtual qreal distanceFromBoundaryTo(const QPointF &pos) const;
    virtual QRectF boundingRect() const;

    qreal x();
    qreal y();

    QPointF point();
};

#endif // Q3POINT_H
