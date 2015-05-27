#include "q3meshinterpolation.h"

Q3MeshTriNodeInterpolation::Q3MeshTriNodeInterpolation(
        Q3Mesh &mesh,
        QVector<QVector3D> &triValues) :
    Q3NaturalNeigbourInterpolation(triValues),
    mesh_(mesh)
{

}

QVector<qreal> Q3MeshTriNodeInterpolation::interpolateToNodes()
{
    QVector<qreal> nodeValues(mesh_.nodes().count());
    qFill(nodeValues, 0);

    for(int i = 0; i < mesh_.nodes().count(); ++i)
    {
        Q3MeshNode *node = mesh_.nodes().at(i);
        if (node->boundary())
        {
            QList<Q3MeshTriangle *> nodeTriangles;
            foreach (Q3MeshEdge *edge, node->edges())
            {
                foreach (Q3MeshTriangle *edgeTriangle, edge->adjacentTriangles())
                {
                    if (!nodeTriangles.contains(edgeTriangle))
                        nodeTriangles.append(edgeTriangle);
                }
            }

            qreal value = 0;
            foreach (Q3MeshTriangle *nodeTriangle, nodeTriangles)
                value += values_[nodeTriangle->id()].z();

            nodeValues[i] = value / nodeTriangles.count();
            continue;
        }

        nodeValues[i] = interpolateToPoint(*node);
    }

    return nodeValues;
}


Q3MeshEdgeNodeInterpolation::Q3MeshEdgeNodeInterpolation(
        Q3Mesh &mesh,
        QVector<QVector3D> &edgeValues) :
    Q3NaturalNeigbourInterpolation(edgeValues),
    mesh_(mesh)
{

}

QVector<qreal> Q3MeshEdgeNodeInterpolation::interpolateToNodes()
{
    QVector<qreal> nodeValues(mesh_.nodes().count());
    qFill(nodeValues, 0);

    for(int i = 0; i < mesh_.nodes().count(); ++i)
    {
        Q3MeshNode *node = mesh_.nodes().at(i);
        if (node->boundary())
        {
            qreal value = 0;
            foreach (Q3MeshEdge *edge, node->edges())
            {
                if (edge->boundary())
                    value += values_[edge->id()].z();
            }
            nodeValues[i] = 0.5 * value;
            continue;
        }

        nodeValues[i] = interpolateToPoint(*node);
    }

    return nodeValues;
}
