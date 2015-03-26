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
    void draw(Q3Painter &painter);
    void move(const QPointF diff);
    qreal distanceTo(const QPointF &pos) const;
    qreal distanceFromBoundaryTo(const QPointF &pos) const;
    QRectF boundingRect() const;

    qreal x();
    qreal y();

    void setX(qreal x);
    void setY(qreal y);

    QPointF point();

    bool accept(Q3ItemVisitor &visitor, Q3SceletonItem *item);
    bool accept(Q3ItemVisitor &visitor, Q3Point *point);
    bool accept(Q3ItemVisitor &visitor, Q3PointConnection *conn);
    bool accept(Q3ItemVisitor &visitor, Q3Circle *circle);

    bool accept(Q3ItemVisitor &visitor);

    void editUI(QFormLayout *layout);

    QString toString();
    QString typeToString();
};

#endif // Q3POINT_H
