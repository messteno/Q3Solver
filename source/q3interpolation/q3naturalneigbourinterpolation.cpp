#include <float.h>
#include "q3naturalneigbourinterpolation.h"

Q3NaturalNeigbourInterpolation::Q3NaturalNeigbourInterpolation(
        Q3Mesh &mesh,
        QVector<qreal> &triValues) :
    Q3MeshTriNodeInterpolation(mesh, triValues),
    delaunay_(NULL),
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
        point pout;
        pout.x = mesh_.nodes().at(i)->x();
        pout.y = mesh_.nodes().at(i)->y();
        nnpi_interpolate_point(interpolator_, &pout);
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
