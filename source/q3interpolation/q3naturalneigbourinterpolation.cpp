#include <float.h>
#include "q3naturalneigbourinterpolation.h"

Q3NaturalNeigbourInterpolation::Q3NaturalNeigbourInterpolation(
        Q3Mesh &mesh,
        QVector<qreal> &triValues) :
    Q3MeshTriNodeInterpolation(mesh, triValues),
    delaunay_(NULL),
    scale_(1),
    interpolator_(NULL)
{
    QVector<point> points;

    for (int i = 0; i < mesh_.triangles().count(); ++i)
    {
        QPointF tp = mesh_.triangles().at(i)->center();
        qreal value = triValues_.at(i);
        point a;
        a.x = tp.x();
        a.y = tp.y();
        a.z = value;
        points.append(a);
    }
    scale_ = points_scaletosquare(points.size(), points.data());

    delaunay_ = delaunay_build(points.size(), points.data(), 0, NULL, 0, NULL);
    interpolator_ = nnpi_create(delaunay_);
    nnpi_setwmin(interpolator_, -DBL_MAX);
}

Q3NaturalNeigbourInterpolation::~Q3NaturalNeigbourInterpolation()
{
    nnpi_destroy(interpolator_);
    delaunay_destroy(delaunay_);
}

QVector<qreal> Q3NaturalNeigbourInterpolation::interpolateToNodes()
{
    QVector<qreal> nodeValues(mesh_.nodes().count());
    qFill(nodeValues, 0);

    for(int i = 0; i < mesh_.nodes().count(); ++i)
    {
        Q3MeshNode *node = mesh_.nodes().at(i);
//        if (node->boundary())
        {
            qreal sum = 0;
            int count = 0;
            foreach (Q3MeshEdge *edge, node->edges())
            {
                sum += triValues_[edge->adjacentTriangles().at(0)->id()];
                count++;
            }
            nodeValues[i] = sum / count;
            continue;
        }

        point pout;
        pout.x = node->x();
        pout.y = node->y();
        points_scale(1, &pout, scale_);
        nnpi_interpolate_point(interpolator_, &pout);
        points_scale(1, &pout, 1. / scale_);

        nodeValues[i] = pout.z;
    }
    return nodeValues;
}

qreal Q3NaturalNeigbourInterpolation::interpolateToPoint(QPointF *pt)
{
    point pout;
    pout.x = pt->x();
    pout.y = pt->y();
    nnpi_interpolate_point(interpolator_, &pout);
    return pout.z;
}
