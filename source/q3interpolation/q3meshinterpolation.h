#ifndef Q3MESHTRINODEINTERPOLATION_H
#define Q3MESHTRINODEINTERPOLATION_H

#include "q3naturalneigbourinterpolation.h"

class Q3MeshTriNodeInterpolation : public Q3NaturalNeigbourInterpolation
{
public:
    Q3MeshTriNodeInterpolation(Q3Mesh &mesh, QVector<QVector3D> &triValues);
    QVector<qreal> interpolateToNodes();

private:
    Q3Mesh &mesh_;
};

class Q3MeshEdgeNodeInterpolation : public Q3NaturalNeigbourInterpolation
{
public:
    Q3MeshEdgeNodeInterpolation(Q3Mesh &mesh, QVector<QVector3D> &edgeValues);
    QVector<qreal> interpolateToNodes();

private:
    Q3Mesh &mesh_;
};

#endif // Q3MESHTRINODEINTERPOLATION_H
