#include <QMap>
#include <QListIterator>
#include <QDebug>

#include "q3itemvisitor.h"
#include "q3meshadapter.h"
#include "q3ani2d.h"
#include "q3point.h"

void Q3Ani2DMeshAdapter::generateMesh(QList<Q3SceletonItem *> &items,
                                      QList<Q3SceletonItem *> &outerBoundary,
                                      QList<QList<Q3SceletonItem *> > \
                                        &innerBoundaries,
                                      QList<Q3SceletonItem *> innerItems)
{
    Q3Ani2D ani2d;
    ani2d.reset();
    ani2d.setMaxElements(1500000);
    ani2d.setQuality(0.9);
    ani2d.setMaxIters(30000);

    // TODO: переделать загрузку границы

    // Создаем карту точек - номеров
    QMap<Q3Point*, int> pointMap;
    int count = 0;
    foreach (Q3SceletonItem *item, items)
    {
        if (item->type() == Q3SceletonItem::Point)
        {
            Q3Point *point = dynamic_cast<Q3Point *>(item);
            if (point)
            {
                pointMap[point] = count++;
                ani2d.addVertex(point->x(), point->y());
            }
        }
    }

    // Определим тип внешней границы
    Q3SceletonItem *firstOuterBoundary = outerBoundary.first();
    Q_ASSERT(firstOuterBoundary);

    int label = 1;
    int domain = 1;
    switch(outerBoundary.first()->type())
    {
        case Q3SceletonItem::Point:
        {
            QList<Q3Point *> boundaryPoints;

            Q3ItemBoundaryClockwiseVisitor clockwiseVisitor;
            foreach (Q3SceletonItem *item, outerBoundary)
            {
                if (item->accept(clockwiseVisitor))
                    boundaryPoints.append(dynamic_cast<Q3Point *>(item));
            }
            outerBoundary.first()->accept(clockwiseVisitor);
            boundaryPoints.append(dynamic_cast<Q3Point *>(outerBoundary.first()));

            bool clockwise = clockwiseVisitor.clockwise();
            QListIterator<Q3Point *> it(boundaryPoints);
            if (clockwise)
                it.toFront();
            else
                it.toBack();

            Q3Point *p1 = NULL;
            Q3Point *p2 = NULL;
            while ((clockwise && it.hasNext()) ||
                   (!clockwise && it.hasPrevious()))
            {
                p2 = clockwise ? it.next() : it.previous();
                if (!p1)
                {
                    p1 = p2;
                    continue;
                }
                ani2d.addEdge(pointMap[p1], pointMap[p2], label, domain);
                p1 = p2;
            }
            break;
        }
        default:
            break;
    }

    QListIterator<QList<Q3SceletonItem *> > bit(innerBoundaries);
    while (bit.hasNext())
    {
        label++;
        QList<Q3SceletonItem *> boundary = bit.next();
        switch(boundary.first()->type())
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

                bool clockwise = clockwiseVisitor.clockwise();
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
                    ani2d.addEdge(pointMap[p1], pointMap[p2], label, domain);
                    p1 = p2;
                }
                break;
            }
            default:
                break;
        }
    }

    label++;
    foreach (Q3SceletonItem *item, innerItems)
    {
        switch (item->type())
        {
            case Q3SceletonItem::PointConnection:
            {
                Q3PointConnection *conn = dynamic_cast<Q3PointConnection *>(item);
                if (conn)
                {
                    ani2d.addEdge(pointMap[conn->a()], pointMap[conn->b()],
                                  label, domain, 1);
                }
                break;
            }
        }
    }

    ani2d.genMeshAnalytic(NULL, NULL);
    ani2d.save ("out.ani", "out.ps");
}
