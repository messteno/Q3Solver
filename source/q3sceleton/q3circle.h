#ifndef Q3CIRCLE_H
#define Q3CIRCLE_H

#include "q3sceletonitem.h"

class Q3Circle : public Q3SceletonItem
{
private:
    QPointF center_;
    qreal radius_;
    static const qreal MinRadius;
public:
    Q3Circle(const QPointF &center, qreal radius);
    virtual ~Q3Circle();

    virtual void draw(Q3Painter &painter);
    virtual qreal distanceTo(const QPointF &pos) const;
    virtual qreal distanceFromBoundaryTo(const QPointF &pos) const;
    virtual QRectF boundingRect() const;
    virtual void move(const QPointF &diff);
    virtual void resize(const QPointF &from, const QPointF &to);

    QPointF center();
    void setCenter(const QPointF &center);

    qreal radius();
    void setRadius(qreal radius);

    bool accept(Q3ItemVisitor &visitor, Q3SceletonItem *item);
    bool accept(Q3ItemVisitor &visitor, Q3Point *point);
    bool accept(Q3ItemVisitor &visitor, Q3PointConnection *conn);
    bool accept(Q3ItemVisitor &visitor, Q3Circle *circle);

    bool accept(Q3ItemVisitor &visitor);

    QString typeToString();
    QString toString();
};

#endif // Q3CIRCLE_H
