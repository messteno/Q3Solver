#ifndef Q3ITEMVISITOR_H
#define Q3ITEMVISITOR_H

#include "q3point.h"
#include "q3pointconnection.h"
#include "q3circle.h"

class Q3ItemVisitor
{
public:
    virtual bool visit(Q3Point *point1, Q3Point *point2) = 0;
    virtual bool visit(Q3Point *point, Q3PointConnection *conn) = 0;
    virtual bool visit(Q3Point *point, Q3Circle *circle) = 0;
    virtual bool visit(Q3PointConnection *conn1, Q3PointConnection *conn2) = 0;
    virtual bool visit(Q3PointConnection *conn, Q3Circle *circle) = 0;
    virtual bool visit(Q3Circle *circle1, Q3Circle *circle2) = 0;

    virtual bool visit(Q3Point *point) = 0;
    virtual bool visit(Q3PointConnection *conn) = 0;
    virtual bool visit(Q3Circle *circle) = 0;
};

class Q3ItemCrossVisitor : public Q3ItemVisitor
{
private:
    static const qreal Precision;
public:
    bool visit(Q3Point *point1, Q3Point *point2);
    bool visit(Q3Point *point, Q3PointConnection *conn);
    bool visit(Q3Point *point, Q3Circle *circle);
    bool visit(Q3PointConnection *conn1, Q3PointConnection *conn2);
    bool visit(Q3PointConnection *conn, Q3Circle *circle);
    bool visit(Q3Circle*circle1, Q3Circle *circle2);

    bool visit(Q3Point *point) { return false; }
    bool visit(Q3PointConnection *conn) { return false; }
    bool visit(Q3Circle *circle) { return false; }
};

class Q3ItemLeftmostVisitor : public Q3ItemVisitor
{
private:
    Q3SceletonItem *leftmost_;
    qreal minLeft_;
    bool inited_;
    bool checkLeftmost(Q3SceletonItem *item);

public:
    Q3ItemLeftmostVisitor();

    bool visit(Q3Point *point1, Q3Point *point2) { return false; }
    bool visit(Q3Point *point, Q3PointConnection *conn) { return false; }
    bool visit(Q3Point *point, Q3Circle *circle) { return false; }
    bool visit(Q3PointConnection *conn1, Q3PointConnection *conn2) { return false; }
    bool visit(Q3PointConnection *conn, Q3Circle *circle) { return false; }
    bool visit(Q3Circle *circle1, Q3Circle *circle2) { return false; }

    bool visit(Q3Point *point);
    bool visit(Q3PointConnection *conn);
    bool visit(Q3Circle *circle);

    Q3SceletonItem *leftmost();
    void reset();
};

class Q3ItemConnectedVisitor : public Q3ItemVisitor
{
private:
    QList<Q3SceletonItem *> connectedItems_;
    QList<Q3SceletonItem *> checkedItems_;
    bool finished_;
    bool started() const;

public:
    Q3ItemConnectedVisitor();

    bool isFinished() const;
    void finish();
    void reset();

    QList<Q3SceletonItem *> connectedItems();
    void backward();

    bool visit(Q3Point *point1, Q3Point *point2) { return false; }
    bool visit(Q3Point *point, Q3PointConnection *conn) { return false; }
    bool visit(Q3Point *point, Q3Circle *circle) { return false; }
    bool visit(Q3PointConnection *conn1, Q3PointConnection *conn2) { return false; }
    bool visit(Q3PointConnection *conn, Q3Circle *circle) { return false; }
    bool visit(Q3Circle *circle1, Q3Circle *circle2) { return false; }

    bool visit(Q3Point *point);
    bool visit(Q3PointConnection *conn);
    bool visit(Q3Circle *circle);
};

class Q3ItemRayTraceVisitor : public Q3ItemVisitor
{
private:
    int rayCrossCount_;
    QPointF checkPoint_;
public:
    Q3ItemRayTraceVisitor(Q3SceletonItem *item);

    int rayCrossCount() const;

    bool visit(Q3Point *point1, Q3Point *point2) { return false; }
    bool visit(Q3Point *point, Q3PointConnection *conn) { return false; }
    bool visit(Q3Point *point, Q3Circle *circle) { return false; }
    bool visit(Q3PointConnection *conn1, Q3PointConnection *conn2) { return false; }
    bool visit(Q3PointConnection *conn, Q3Circle *circle) { return false; }
    bool visit(Q3Circle *circle1, Q3Circle *circle2) { return false; }

    bool visit(Q3Point *point);
    bool visit(Q3PointConnection *conn);
    bool visit(Q3Circle *circle);
};

class Q3ItemInnerBoundaryVisitor : public Q3ItemVisitor
{
private:
    QList<Q3SceletonItem *> items_;
    QList<Q3SceletonItem *> boundary_;
public:
    Q3ItemInnerBoundaryVisitor(QList<Q3SceletonItem *> items);

    QList<Q3SceletonItem *> getBoundary() const;

    bool visit(Q3Point *point1, Q3Point *point2) { return false; }
    bool visit(Q3Point *point, Q3PointConnection *conn) { return false; }
    bool visit(Q3Point *point, Q3Circle *circle) { return false; }
    bool visit(Q3PointConnection *conn1, Q3PointConnection *conn2) { return false; }
    bool visit(Q3PointConnection *conn, Q3Circle *circle) { return false; }
    bool visit(Q3Circle *circle1, Q3Circle *circle2) { return false; }

    bool visit(Q3Point *point);
    bool visit(Q3PointConnection *conn);
    bool visit(Q3Circle *circle);
};

class Q3ItemBoundaryClockwiseVisitor : public Q3ItemVisitor
{
private:
    qreal square_;
    Q3Point *previous_;
public:
    Q3ItemBoundaryClockwiseVisitor();
    bool clockwise() const;
    qreal square();

    bool visit(Q3Point *point1, Q3Point *point2) { return false; }
    bool visit(Q3Point *point, Q3PointConnection *conn) { return false; }
    bool visit(Q3Point *point, Q3Circle *circle) { return false; }
    bool visit(Q3PointConnection *conn1, Q3PointConnection *conn2) { return false; }
    bool visit(Q3PointConnection *conn, Q3Circle *circle) { return false; }
    bool visit(Q3Circle *circle1, Q3Circle *circle2) { return false; }

    bool visit(Q3Point *point);
    bool visit(Q3PointConnection *conn);
    bool visit(Q3Circle *circle);
};

#endif // Q3ITEMVISITOR_H
