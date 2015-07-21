#include <math.h>
#include <QString>
#include <QFileDialog>
#include <QDir>
#include <QTime>
#include <QMap>
#include <QListIterator>
#include <QDebug>

#include "q3itemvisitor.h"
#include "q3ani2dmeshadapter.h"
#include "q3point.h"

static QList<Q3SceletonItem *> boundaryItems_;
static qreal elementSize = 0.0;
static qreal elementSizeDecrease = 0.0;
static qreal maxDistToBoundaryEstimation = 0.0;
static QVector<Q3Ani2DMeshAdapter::CurveBoundary> curves_;

Q3Ani2DMeshAdapter::Q3Ani2DMeshAdapter() :
    Q3MeshAdapter()
{
}

bool Q3Ani2DMeshAdapter::generateMesh(Q3Sceleton &sceleton,
                                      QList<Q3Boundary *> &boundaries)
{
    if (!sceleton.isPrepared())
        return false;

    created_ = false;
    curves_.clear();

    // TODO: описать алгоритм
    labelBoundaryDelimeters_.clear();

    QList<Q3SceletonItem *> &items = sceleton.items();
    QList<Q3SceletonItem *> &outerBoundary = sceleton.outerBoundary();
    QList<QList<Q3SceletonItem *> > &innerBoundaries = sceleton.innerBoundaries();
    QList<Q3SceletonItem *> &innerItems = sceleton.innerElements();

    q3ani2d_.reset();
    q3ani2d_.setMaxElements(1500000);
    q3ani2d_.setQuality(0.95);
    q3ani2d_.setMaxIters(50000);

    boundaryItems_ = items;

    // Создаем отображение точек - номеров
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
    qreal boundarySquare = 0;
    bool ret = addBoundary(boundaries, outerBoundary, boundarySquare, true);
    if (!ret)
        return false;
    square += boundarySquare;

    QListIterator<QList<Q3SceletonItem *> > bit(innerBoundaries);
    while (bit.hasNext())
    {
        QList<Q3SceletonItem *> innerBoundary = bit.next();
        boundarySquare = 0;
        bool ret = addBoundary(boundaries, innerBoundary, boundarySquare, false);
        if (!ret)
            return false;
        square -= boundarySquare;
    }

    bool lastDelimeter = false;

    foreach (Q3SceletonItem *item, innerItems)
    {
        switch (item->type())
        {
            case Q3SceletonItem::PointConnection:
            {
                Q3PointConnection *conn = dynamic_cast<Q3PointConnection *>(item);
                Q_ASSERT(conn);

                int label = -1;
                Q3Boundary *boundary = Q3Boundary::findByElement(boundaries,
                                                                 item);
                if (boundary)
                {
                    label = boundary->label();
                    if (lastDelimeter == false)
                    {
                        labelBoundaryDelimeters_.append(label);
                        lastDelimeter = true;
                    }
                }

                q3ani2d_.addEdge(pointMap_[conn->a()], pointMap_[conn->b()],
                        label, 1, 1);
                break;
            }
        }
    }

    if (lastDelimeter == false)
    {
        labelBoundaryDelimeters_.append(-1);
        lastDelimeter = true;
    }

    elementSizeDecrease = elementSizeDecrease_;

    bool first = true;

    foreach (Q3SceletonItem *itemPC, items)
    {
        if (itemPC->type() == Q3SceletonItem::PointConnection ||
                itemPC->type() == Q3SceletonItem::Circle)
        {
            qreal curMaxDist = 0.0;
            foreach (Q3SceletonItem *itemP, items)
            {
                if (itemP->type() == Q3SceletonItem::Point)
                {
                    Q3Point *point = dynamic_cast<Q3Point *>(itemP);
                    if (point)
                    {
                        qreal t = 0.5*itemPC->distanceFromBoundaryTo(point->point());
                        if (t > curMaxDist) curMaxDist = t;
                    }
                }
            }

            if (first)
            {
                maxDistToBoundaryEstimation = curMaxDist;
                first = false;
            }
            else
            {
                if (maxDistToBoundaryEstimation > curMaxDist)
                    maxDistToBoundaryEstimation = curMaxDist;
            }
        }
    }

    switch(sizePolicy_)
    {
        case ElementSizeByCount:
            elementSize = sqrt(square * 2. / elementsCount_);
            break;
        case ElementSizeBySize:
            elementSize = elementSize_;
            break;
        case ElementSizeAuto:
        default:
            elementSize = sqrt(square / 50000);
            break;
    }

    created_ = q3ani2d_.genMeshAnalytic(Q3Ani2DMeshAdapter::sizeFunction,
                                        Q3Ani2DMeshAdapter::circleBoundary,
                                        elementSize);
    return created_;
}

bool Q3Ani2DMeshAdapter::meshToQ3Mesh(Q3Mesh &mesh, QList<Q3Boundary *> &boundaries)
{
    if (!created_)
        return false;

    mesh.clear();
    ani2D& ani = q3ani2d_.getAni2D();

    for (int i = 0; i < ani.nv; ++i)
        mesh.addNode(ani.vrt[2 * i], ani.vrt[2 * i + 1]);

    int delimeterLabel = -1;
    if (!labelBoundaryDelimeters_.empty())
        delimeterLabel = labelBoundaryDelimeters_.first();

    for (int i = 0; i < ani.nb; ++i)
    {
        Q3MeshNode *a = mesh.nodes()[ani.bnd[2 * i + 0] - 1];
        Q3MeshNode *b = mesh.nodes()[ani.bnd[2 * i + 1] - 1];
        int label = ani.labelB[i];

        Q3Boundary *boundary = Q3Boundary::findByLabel(boundaries, label);
        Q3MeshEdge *ab = mesh.addEdge(a, b, boundary);

        if (delimeterLabel == label)
        {
            delimeterLabel = -1;
            if (!labelBoundaryDelimeters_.empty())
                labelBoundaryDelimeters_.removeFirst();
            if (!labelBoundaryDelimeters_.empty())
            {
                delimeterLabel = labelBoundaryDelimeters_.first();
                mesh.boundaries().append(QList<Q3MeshEdge *>());
            }
        }

        if (!mesh.boundaries().isEmpty() && !labelBoundaryDelimeters_.isEmpty())
        {
            QList<Q3MeshEdge *>& edgeBoundary = mesh.boundaries().back();
            edgeBoundary.append(ab);
        }
    }

    for (int i = 0; i < ani.nt; ++i)
    {
        Q3MeshNode *a = mesh.nodes().at(ani.tri[3 * i + 0] - 1);
        Q3MeshNode *b = mesh.nodes().at(ani.tri[3 * i + 1] - 1);
        Q3MeshNode *c = mesh.nodes().at(ani.tri[3 * i + 2] - 1);

        Q3MeshEdge *ab = mesh.addEdge(a, b, 0);
        Q3MeshEdge *bc = mesh.addEdge(b, c, 0);
        Q3MeshEdge *ac = mesh.addEdge(a, c, 0);

        mesh.addTriangle(ab, bc, ac);
    }
    mesh.update();

    emit meshCreated();

    return true;
}

bool Q3Ani2DMeshAdapter::saveMesh()
{
    if (!created_)
        return false;
    QString fileName = QFileDialog::getSaveFileName(NULL, QObject::tr("Save File"),
                                                    QDir::homePath(),
                                                    QObject::tr("*.ani"));
    fileName += ".ani";
    q3ani2d_.save(fileName);
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

bool Q3Ani2DMeshAdapter::addBoundary(QList<Q3Boundary *> &boundaries,
                                     QList<Q3SceletonItem *> &boundary,
                                     qreal &square, bool outer)
{
    if (boundary.empty())
        return false;

    Q3SceletonItem *firstItem = boundary.first();
    bool boundaryDelimeterAdded = false;

    switch(firstItem->type())
    {
        case Q3SceletonItem::Point:
        {
            Q3ItemBoundaryClockwiseVisitor clockwiseVisitor;
            foreach (Q3SceletonItem *item, boundary)
                item->accept(clockwiseVisitor);

            bool aa = clockwiseVisitor.clockwise();
            bool bb = outer;
            bool clockwise = aa ^ bb;
            QListIterator<Q3SceletonItem *> it(boundary);
            if (!clockwise)
                it.toFront();
            else
                it.toBack();

            Q3Point *p1 = NULL;
            Q3Point *p2 = NULL;

            int label = -1;
            while ((!clockwise && it.hasNext()) ||
                   (clockwise && it.hasPrevious()))
            {
                Q3SceletonItem *item = !clockwise ? it.next() : it.previous();
                if (item->type() == Q3SceletonItem::Point)
                {
                    p2 = dynamic_cast<Q3Point *>(item);
                    Q_ASSERT(p2);

                    if (!p1)
                    {
                        p1 = p2;
                        continue;
                    }
                    if (label == -1)
                        return false;
                    q3ani2d_.addEdge(pointMap_[p1], pointMap_[p2], label, 1);
                    label = -1;
                    p1 = p2;
                }
                else
                {
                    Q3Boundary *bnd = Q3Boundary::findByElement(boundaries,
                                                                item);
                    if (!bnd)
                        return false;
                    label = bnd->label();

                    if (!boundaryDelimeterAdded)
                    {
                        labelBoundaryDelimeters_.append(label);
                        boundaryDelimeterAdded = true;
                    }
                }
            }

            square = clockwiseVisitor.square();
            break;
        }
        case Q3SceletonItem::Circle:
        {
            int label = -1;
            Q3Boundary *bnd = Q3Boundary::findByElement(boundaries, firstItem);
            if (!bnd)
                return false;
            label = bnd->label();

            if (!boundaryDelimeterAdded)
            {
                labelBoundaryDelimeters_.append(label);
                boundaryDelimeterAdded = true;
            }

            qreal left = firstItem->boundingRect().left();
            qreal right = firstItem->boundingRect().right();
            qreal bottom = firstItem->boundingRect().bottom();
            qreal top = firstItem->boundingRect().top();

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
            cb.item = firstItem;
            cb.outer = outer;
            curves_.push_back(cb);

            square = 0.25 * M_PI * (right - left) * (right - left);
            break;
        }
        default:
            return false;
    }
    return true;
}

double Q3Ani2DMeshAdapter::sizeFunction(double *point)
{
    double hmin = elementSize;
    double hmax = elementSize / (1.0 - elementSizeDecrease * 0.01);
//    double hmax = 0.1;
//    double hmin = 0.02;

//    qreal minDist = sqrt((point[0] - 10) * (point[0] - 10)
//            + (point[1] - 10) * (point[1] - 10)) - 0.5;

    double minDist = distToBoundary(point);
    double coef = minDist * 2;
    coef = coef > 1 ? 1 : coef;
    coef *= coef;
    return (1 - coef) * hmin + coef * hmax;
//    return elementSize;
}

double Q3Ani2DMeshAdapter::distToBoundary(double *point)
{
    QPointF p(point[0], point[1]);
    double minDist = 0.0;
    bool first = true;

    foreach (Q3SceletonItem *item, boundaryItems_)
    {
        double dist = item->distanceFromBoundaryTo(p);

        if (first)
        {
            minDist = dist;
            first = false;
        }
        else
        {
            if (dist < minDist)
                minDist = dist;
        }
    }

    return minDist;
}

