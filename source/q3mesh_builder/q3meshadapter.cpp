#include <math.h>

#include <QMap>
#include <QListIterator>
#include <QDebug>

#include "q3itemvisitor.h"
#include "q3meshadapter.h"
#include "q3point.h"

static QVector<Q3Ani2DMeshAdapter::CurveBoundary> curves_;

Q3MeshAdapter::Q3MeshAdapter() :
    created_(false),
    sizePolicy_(ElementSizeAuto),
    elementsCount_(20000),
    elementSize_(0.01)
{

}

void Q3MeshAdapter::setSizePolicy(const SizePolicy &sizePolicy)
{
    sizePolicy_ = sizePolicy;
}

void Q3MeshAdapter::setElementsCount(int elementsCount)
{
    elementsCount_ = elementsCount;
}

void Q3MeshAdapter::setElementSize(qreal elementSize)
{
    elementSize_ = elementSize;
}

Q3Ani2DMeshAdapter::Q3Ani2DMeshAdapter() :
    Q3MeshAdapter()
{
}

bool Q3Ani2DMeshAdapter::generateMesh(QList<Q3SceletonItem *> &items,
                                      QList<Q3SceletonItem *> &outerBoundary,
                                      QList<QList<Q3SceletonItem *> > \
                                        &innerBoundaries,
                                      QList<Q3SceletonItem *> &innerItems)
{
    created_ = false;

    curves_.clear();

    // передавать размер ячейки
    // определить площадь
    // вычислить количество элементов

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

    qreal square = 0;
    int label = 1;
    qreal boundarySquare = 0;
    addBoundary(outerBoundary, boundarySquare, true, label);
    square += boundarySquare;

    QListIterator<QList<Q3SceletonItem *> > bit(innerBoundaries);
    while (bit.hasNext())
    {
        label++;
        QList<Q3SceletonItem *> boundary = bit.next();
        boundarySquare = 0;
        addBoundary(boundary, boundarySquare, false, label);
        square -= boundarySquare;
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

    qreal elementSize = 0;
    switch(sizePolicy_)
    {
        case ElementSizeByCount:
            elementSize = sqrt(square / 2. / elementsCount_);
            break;
        case ElementSizeBySize:
            elementSize = elementSize_;
            break;
        case ElementSizeAuto:
        default:
            elementSize = sqrt(square / 50000);
            break;
    }

    created_ = q3ani2d_.genMeshAnalytic(NULL,
                                        Q3Ani2DMeshAdapter::circleBoundary,
                                        elementSize);
    return created_;
}

bool Q3Ani2DMeshAdapter::meshToQ3Mesh(Q3Mesh *mesh)
{
    if (!created_)
        return false;

    mesh->clear();
    ani2D& ani = q3ani2d_.getAni2D();

    for (int i = 0; i < ani.nv; ++i)
        mesh->addNode(ani.vrt[2 * i], ani.vrt[2 * i + 1]);

    qDebug() << ani.nt;
    for (int i = 0; i < ani.nt; ++i)
    {
        Q3MeshNode *a = &mesh->nodes()[ani.tri[3 * i + 0] - 1];
        Q3MeshNode *b = &mesh->nodes()[ani.tri[3 * i + 1] - 1];
        Q3MeshNode *c = &mesh->nodes()[ani.tri[3 * i + 2] - 1];

        if (!a->adjacentTo(b))
            mesh->addEdge(a, b);

        if (!b->adjacentTo(c))
            mesh->addEdge(b, c);

        if (!c->adjacentTo(a))
            mesh->addEdge(c, a);
    }

    return true;
}

void Q3Ani2DMeshAdapter::circleBoundary(int *param, double *t,
                                        double *x, double *y)
{
    int curveId = *param - 1;
    CurveBoundary boundary = curves_[curveId];
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
                                     qreal &square, bool outer, int label)
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

            square = clockwiseVisitor.square();
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

            int curveId = curves_.size() + 1;

            if (outer)
                std::swap(v1, v3);
            q3ani2d_.addCurveEdge(v0, v1, 0, 0.5 * M_PI,
                                  label, curveId, 1, 0);
            q3ani2d_.addCurveEdge(v1, v2, 0.5 * M_PI, M_PI,
                                  label, curveId, 1, 0);
            q3ani2d_.addCurveEdge(v2, v3, M_PI, 1.5 * M_PI,
                                  label, curveId, 1, 0);
            q3ani2d_.addCurveEdge(v3, v0, 1.5 * M_PI, 2 * M_PI,
                                  label, curveId, 1, 0);

            CurveBoundary cb;
            cb.item = item;
            cb.outer = outer;
            curves_.push_back(cb);

            square = 0.25 * M_PI * (right - left) * (right - left);
            break;
        }
        default:
            break;
    }
}
