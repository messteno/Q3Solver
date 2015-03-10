#ifndef Q3POINT_H
#define Q3POINT_H

#include "q3sceletonitem.h"
#include "q3painter.h"

class Q3PointConnection;

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

    bool accept(Q3ItemVisitor &visitor, Q3SceletonItem *item);
    bool accept(Q3ItemVisitor &visitor, Q3Point *point);
    bool accept(Q3ItemVisitor &visitor, Q3PointConnection *conn);
    bool accept(Q3ItemVisitor &visitor, Q3Circle *circle);

    bool accept(Q3ItemVisitor &visitor);
};

#endif // Q3POINT_H
