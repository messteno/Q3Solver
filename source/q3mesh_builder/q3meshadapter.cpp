#include <math.h>

#include <QMap>
#include <QListIterator>
#include <QDebug>

#include "q3itemvisitor.h"
#include "q3meshadapter.h"
#include "q3point.h"

typedef struct
{
    Q3SceletonItem *item;
    bool outer;
} CurveBoundary;

static QVector<CurveBoundary> curves;

bool Q3Ani2DMeshAdapter::generateMesh(QList<Q3SceletonItem *> &items,
                                      QList<Q3SceletonItem *> &outerBoundary,
                                      QList<QList<Q3SceletonItem *> > \
                                        &innerBoundaries,
                                      QList<Q3SceletonItem *> innerItems)
{
    curves.clear();

    q3ani2d_.reset();
    q3ani2d_.setMaxElements(1500000);
    q3ani2d_.setQuality(0.9);
    q3ani2d_.setMaxIters(30000);

    // Создаем карту точек - номеров
    int count = 0;
    foreach (Q3SceletonItem *item, items)
    {
        if (item->type() == Q3SceletonItem::Point)
        {
            Q3Point *point = dynamic_cast<Q3Point *>(item);
            if (point)
            {
                pointMap_[point] = count++;
                q3ani2d_.addVertex(point->x(), point->y());
            }
        }
    }

    int label = 1;
    addBoundary(outerBoundary, true, label);

    QListIterator<QList<Q3SceletonItem *> > bit(innerBoundaries);
    while (bit.hasNext())
    {
        label++;
        QList<Q3SceletonItem *> boundary = bit.next();
        addBoundary(boundary, false, label);
    }

    label++;
    foreach (Q3SceletonItem *item, innerItems)
    {
        switch (item->type())
        {
            case Q3SceletonItem::PointConnection:
            {
                Q3PointConnection *conn = \
                        dynamic_cast<Q3PointConnection *>(item);
                if (conn)
                {
                    q3ani2d_.addEdge(pointMap_[conn->a()], pointMap_[conn->b()],
                                   label, 1, 1);
                }
                break;
            }
        }
    }

    bool success = q3ani2d_.genMeshAnalytic(NULL,
                                            Q3Ani2DMeshAdapter::circleBoundary);

    return success;
}

void Q3Ani2DMeshAdapter::circleBoundary(int *param, double *t,
                                        double *x, double *y)
{
    int curveId = *param - 1;
    CurveBoundary boundary = curves[curveId];
    switch (boundary.item->type())
    {
        case Q3SceletonItem::Circle:
        {
            Q3Circle* circle = dynamic_cast<Q3Circle *>(boundary.item);
            Q_ASSERT(circle);

            *x = circle->center().x() + circle->radius() * cos(*t);
            *y = circle->center().y();
            *y += circle->radius() * sin(*t) * (boundary.outer ? -1 : 1);
            break;
        }
        default:
            break;
    }
}

void Q3Ani2DMeshAdapter::addBoundary(QList<Q3SceletonItem *> &boundary,
                                     bool outer, int label)
{
    if (boundary.empty())
        return;

    Q3SceletonItem *item = boundary.first();

    switch(item->type())
    {
        case Q3SceletonItem::Point:
        {
            QList<Q3Point *> boundaryPoints;

            Q3ItemBoundaryClockwiseVisitor clockwiseVisitor;
            foreach (Q3SceletonItem *item, boundary)
            {
                if (item->accept(clockwiseVisitor))
                    boundaryPoints.append(dynamic_cast<Q3Point *>(item));
            }
            boundary.first()->accept(clockwiseVisitor);
            boundaryPoints.append(dynamic_cast<Q3Point *>(boundary.first()));

            bool clockwise = clockwiseVisitor.clockwise() ^ outer;
            QListIterator<Q3Point *> it(boundaryPoints);
            if (!clockwise)
                it.toFront();
            else
                it.toBack();

            Q3Point *p1 = NULL;
            Q3Point *p2 = NULL;
            while ((!clockwise && it.hasNext()) ||
                   (clockwise && it.hasPrevious()))
            {
                p2 = !clockwise ? it.next() : it.previous();
                if (!p1)
                {
                    p1 = p2;
                    continue;
                }
                q3ani2d_.addEdge(pointMap_[p1], pointMap_[p2], label, 1);
                p1 = p2;
            }
            break;
        }
        case Q3SceletonItem::Circle:
        {
            qreal left = item->boundingRect().left();
            qreal right = item->boundingRect().right();
            qreal bottom = item->boundingRect().bottom();
            qreal top = item->boundingRect().top();

            int v0 = q3ani2d_.addVertex(left, 0.5 * (bottom + top));
            int v1 = q3ani2d_.addVertex(0.5 * (left + right), top);
            int v2 = q3ani2d_.addVertex(right, 0.5 * (bottom + top));
            int v3 = q3ani2d_.addVertex(0.5 * (left + right), bottom);

            int curveId = curves.size() + 1;

            if (outer)
                std::swap(v1, v3);
            q3ani2d_.addCurveEdge(v0, v1, 0, 0.5 * M_PI, label, curveId, 1, 0);
            q3ani2d_.addCurveEdge(v1, v2, 0.5 * M_PI, M_PI, label, curveId, 1, 0);
            q3ani2d_.addCurveEdge(v2, v3, M_PI, 1.5 * M_PI, label, curveId, 1, 0);
            q3ani2d_.addCurveEdge(v3, v0, 1.5 * M_PI, 2 * M_PI, label, curveId, 1, 0);

            CurveBoundary cb;
            cb.item = item;
            cb.outer = outer;
            curves.push_back(cb);

            break;
        }
        default:
            break;
    }
}
