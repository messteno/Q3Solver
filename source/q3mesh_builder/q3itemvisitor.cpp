#include <math.h>
#include <qmath.h>
#include <algorithm>

#include <QDebug>
#include <QVector2D>

#include "q3itemvisitor.h"

const qreal Q3ItemCrossVisitor::Precision = 1e-10;

bool Q3ItemCrossVisitor::visit(Q3Point *point1, Q3Point *point2)
{
    qreal x1 = point1->x();
    qreal y1 = point1->y();
    qreal x2 = point2->x();
    qreal y2 = point2->y();
    return (qAbs(x1 - x2) < Precision) && (qAbs(y1 - y2) < Precision);
}

bool Q3ItemCrossVisitor::visit(Q3Point *point, Q3PointConnection *conn)
{
    if (conn->a() == point || conn->b() == point)
        return false;

    return conn->distanceTo(point->point()) < Precision;
}

bool Q3ItemCrossVisitor::visit(Q3Point *point, Q3Circle *circle)
{
    return qAbs(circle->distanceTo(point->point())
                - circle->radius()) < Precision;
}

bool Q3ItemCrossVisitor::visit(Q3PointConnection *conn1,
                               Q3PointConnection *conn2)
{
    QPointF a1 = conn1->a()->point();
    QPointF b1 = conn1->b()->point();
    QPointF a2 = conn2->a()->point();
    QPointF b2 = conn2->b()->point();

    QVector2D v1(b2 - a1);
    QVector2D v2(a2 - a1);
    QVector2D v3(b2 - b1);
    QVector2D v4(a2 - b1);

    qreal ccw1 = v1.x() * v2.y() - v1.y() * v2.x();
    qreal ccw2 = v3.x() * v4.y() - v3.y() * v4.x();
    qreal ccw3 = v1.x() * v3.y() - v1.y() * v3.x();
    qreal ccw4 = v2.x() * v4.y() - v2.y() * v4.x();

    return (ccw1 * ccw2 < 0) && (ccw3 * ccw4 < 0);
}

bool Q3ItemCrossVisitor::visit(Q3PointConnection *conn, Q3Circle *circle)
{
    QVector2D v1(conn->a()->point() - circle->center());
    QVector2D v2(conn->b()->point() - circle->center());

    qreal radius = circle->radius();

    if (v1.length() < radius && v2.length() < radius)
        return false;

    if (conn->distanceTo(circle->center()) > radius)
        return false;

    return true;
}

bool Q3ItemCrossVisitor::visit(Q3Circle *circle1, Q3Circle *circle2)
{
    qreal r1 = circle1->radius();
    qreal r2 = circle2->radius();

    QVector2D c1c2(circle1->center() - circle2->center());
    qreal dist = c1c2.length();

    return (dist <= r1 + r2) && (dist >= qAbs(r1 - r2));
}


Q3ItemLeftmostVisitor::Q3ItemLeftmostVisitor() :
    inited_(false),
    leftmost_(NULL),
    minLeft_(0)
{

}

bool Q3ItemLeftmostVisitor::visit(Q3Point *point)
{
    return checkLeftmost(point);
}

bool Q3ItemLeftmostVisitor::visit(Q3PointConnection *conn)
{
    // Не будем рассматривать самый левый отрезок, ищем точку
    return false;
}

bool Q3ItemLeftmostVisitor::visit(Q3Circle *circle)
{
    return checkLeftmost(circle);
}

bool Q3ItemLeftmostVisitor::checkLeftmost(Q3SceletonItem *item)
{
    qreal left = item->boundingRect().left();
    if (left < minLeft_ || !inited_)
    {
        inited_ = true;
        minLeft_ = left;
        leftmost_ = item;
        return true;
    }
    return false;
}

Q3SceletonItem *Q3ItemLeftmostVisitor::leftmost()
{
    return leftmost_;
}

void Q3ItemLeftmostVisitor::reset()
{
    inited_ = false;
    minLeft_ = 0;
    leftmost_ = NULL;
}


bool Q3ItemConnectedVisitor::started() const
{
    return (!connectedItems_.empty() || !checkedItems_.empty());
}

Q3ItemConnectedVisitor::Q3ItemConnectedVisitor() :
    finished_(false)
{

}

bool Q3ItemConnectedVisitor::isFinished() const
{
    return finished_;
}

void Q3ItemConnectedVisitor::finish()
{
    finished_ = true;
}

void Q3ItemConnectedVisitor::reset()
{
    connectedItems_.clear();
    finished_ = false;
}

QList<Q3SceletonItem *> Q3ItemConnectedVisitor::connectedItems()
{
    return connectedItems_;
}

void Q3ItemConnectedVisitor::backward()
{
    if (!connectedItems_.empty())
        connectedItems_.pop_back();
    if (!connectedItems_.empty())
        connectedItems_.pop_back();
    if (connectedItems_.empty())
        finish();
}

bool Q3ItemConnectedVisitor::visit(Q3Point *point)
{
    if (!started())
    {
        connectedItems_.append(point);
        return true;
    }
    return false;
}

bool Q3ItemConnectedVisitor::visit(Q3PointConnection *conn)
{
    if (!started())
    {
        connectedItems_.append(conn);
        connectedItems_.prepend(conn->a());
        connectedItems_.append(conn->b());
        return true;
    }

    if (checkedItems_.contains(conn))
        return false;

    if (connectedItems_.contains(conn))
        return false;

    Q3SceletonItem *lastItem = connectedItems_.last();
    Q3SceletonItem *firstItem = connectedItems_.first();
    Q_ASSERT(lastItem->type() == Q3SceletonItem::Point);

    if (conn->a() == lastItem)
    {
        checkedItems_.append(conn);
        connectedItems_.append(conn);

        if (conn->b() == firstItem)
            finish();
        else
            connectedItems_.append(conn->b());

        return true;
    }
    else if (conn->b() == lastItem)
    {
        checkedItems_.append(conn);
        connectedItems_.append(conn);

        if (conn->a() == firstItem)
            finish();
        else
            connectedItems_.append(conn->a());

        return true;
    }

    // Не будем проверять подходит ли отрезок для
    // первго элемента цепочки, потому что необходимо,
    // чтобы первый элемент был самым левым

    return false;
}

bool Q3ItemConnectedVisitor::visit(Q3Circle *circle)
{
    if (!started())
    {
        connectedItems_.append(circle);
        finish();
        return true;
    }

    return false;
}


Q3ItemRayTraceVisitor::Q3ItemRayTraceVisitor(Q3SceletonItem *item) :
    rayCrossCount_(0)
{
    checkPoint_ = item->boundingRect().center();
}

int Q3ItemRayTraceVisitor::rayCrossCount() const
{
    return rayCrossCount_;
}

bool Q3ItemRayTraceVisitor::visit(Q3Point *point)
{
    return false;
}

bool Q3ItemRayTraceVisitor::visit(Q3PointConnection *conn)
{
    QPointF top = conn->a()->point();
    QPointF bottom = conn->b()->point();

    if (top.y() < bottom.y())
        std::swap(top, bottom);

    if (checkPoint_.y() > top.y() ||
        checkPoint_.y() < bottom.y())
    {
        return false;
    }

    QVector2D v1(top - checkPoint_);
    QVector2D v2(bottom - checkPoint_);

    if (v1.x() * v2.y() - v1.y() * v2.x() < 0)
    {
        if (top.y() > checkPoint_.y())
            rayCrossCount_++;
        return true;
    }

    return false;
}

bool Q3ItemRayTraceVisitor::visit(Q3Circle *circle)
{
    QVector2D vec(checkPoint_ - circle->center());
    if (vec.length() < circle->radius())
    {
        rayCrossCount_++;
        return true;
    }

    return false;
}


Q3ItemInnerBoundaryVisitor::Q3ItemInnerBoundaryVisitor(
        QList<Q3SceletonItem *> items) :
    items_(items)
{

}

QList<Q3SceletonItem *> Q3ItemInnerBoundaryVisitor::getBoundary() const
{
    return boundary_;
}

bool Q3ItemInnerBoundaryVisitor::visit(Q3Point *point)
{
    QMap<Q3Point*, QList<Q3PointConnection*> > pointConnMap;
    foreach (Q3SceletonItem *item, items_)
    {
        if (item->type() == Q3SceletonItem::PointConnection)
        {
            Q3PointConnection *conn = dynamic_cast<Q3PointConnection*>(item);
            if (conn)
            {
                pointConnMap[conn->a()].append(conn);
                pointConnMap[conn->b()].append(conn);
            }
        }
    }

    boundary_.append(point);
    Q3Point *lastPoint = point;
    Q3PointConnection *lastConn = NULL;
    while(!boundary_.empty())
    {
        Q_ASSERT(lastPoint);

        Q3PointConnection *maxAngleConnection = NULL;
        qreal maxAngle = 0;
        foreach (Q3PointConnection *conn, pointConnMap[lastPoint])
        {
            QVector2D v1(0, -1);
            if (lastConn)
            {
                if (lastPoint == lastConn->a())
                    v1 = QVector2D(lastConn->b()->point() - lastPoint->point());
                else
                    v1 = QVector2D(lastConn->a()->point() - lastPoint->point());
            }

            QVector2D v2(0, 1);
            if (conn)
            {
                if (lastPoint == conn->a())
                    v2 = QVector2D(conn->b()->point() - lastPoint->point());
                else
                    v2 = QVector2D(conn->a()->point() - lastPoint->point());
            }

            qreal angle = acos(QVector2D::dotProduct(v1, v2)
                               / v1.length() / v2.length());
            if (v1.x() * v2.y() - v1.y() * v2.x() < 0)
                angle = 2 * M_PI - angle;

            if (!maxAngleConnection || maxAngle < angle)
            {
                maxAngleConnection = conn;
                maxAngle = angle;
            }
        }

        if (!maxAngleConnection)
        {
            if (!boundary_.empty())
                boundary_.removeLast();
            if (!boundary_.empty())
                boundary_.removeLast();

            if (!boundary_.empty())
            {
                lastPoint = dynamic_cast<Q3Point*>(boundary_.takeLast());
                if (!boundary_.empty())
                    lastConn = dynamic_cast<Q3PointConnection*>(boundary_.last());
                boundary_.append(lastPoint);
            }
            continue;
        }

        lastConn = maxAngleConnection;
        pointConnMap[lastPoint].removeAll(lastConn);
        boundary_.append(lastConn);

        if (maxAngleConnection->a() == lastPoint)
            lastPoint = maxAngleConnection->b();
        else
            lastPoint = maxAngleConnection->a();

        if (lastPoint == boundary_.first())
            break;

        pointConnMap[lastPoint].removeAll(lastConn);
        boundary_.append(lastPoint);
    }

    if (boundary_.empty())
        return false;
    return true;
}

bool Q3ItemInnerBoundaryVisitor::visit(Q3PointConnection *conn)
{
    return false;
}

bool Q3ItemInnerBoundaryVisitor::visit(Q3Circle *circle)
{
    if (!items_.contains(circle))
        return false;
    boundary_.append(circle);
    return true;
}


Q3ItemBoundaryClockwiseVisitor::Q3ItemBoundaryClockwiseVisitor() :
    square_(0),
    previous_(NULL)
{

}

bool Q3ItemBoundaryClockwiseVisitor::clockwise() const
{
    return square_ < 0;
}

qreal Q3ItemBoundaryClockwiseVisitor::square()
{
    return fabs(square_);
}

bool Q3ItemBoundaryClockwiseVisitor::visit(Q3Point *point)
{
    if (!previous_)
    {
        previous_ = point;
        return true;
    }
    square_ += 0.5 * (previous_->x() * point->y() - previous_->y() * point->x());
    previous_ = point;
    return true;
}

bool Q3ItemBoundaryClockwiseVisitor::visit(Q3PointConnection *conn)
{
    return false;
}

bool Q3ItemBoundaryClockwiseVisitor::visit(Q3Circle *circle)
{
    return false;
}
