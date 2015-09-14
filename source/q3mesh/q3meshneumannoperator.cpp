#include <QDebug>

#include "q3meshneumannoperator.h"

Q3MeshNeumannOperator::Q3MeshNeumannOperator(Q3Mesh &mesh, int size) :
    LinearOperator(size),
    mesh_(mesh)
{

}

Q3Vector Q3MeshNeumannOperator::operator *(const Q3Vector &vec) const
{
    Q_ASSERT(size_ == vec.size());

    Q3Vector result(size_);
    int trCount = mesh_.triangles().count();

    qreal sum = 0;
    for (int trInd = 0; trInd < trCount; ++trInd)
    {
        Q3MeshTriangle *triangle = mesh_.triangles().at(trInd);
        qreal A = 0;
        qreal B = 0;

        for (int eInd = 0; eInd < triangle->edges().size(); ++eInd)
        {
            Q3MeshEdge *edge = triangle->edges().at(eInd);
            Q3MeshTriangle *adjTriangle = triangle->adjacentTriangles().at(eInd);
            QVector2D normal = triangle->normalVectors().at(eInd);

            if (adjTriangle)
            {
                qreal dL = triangle->distanceToTriangles().at(eInd);
                QVector2D tAt(adjTriangle->center() - triangle->center());
                tAt.normalize();

                qreal cosin = qAbs(QVector2D::dotProduct(tAt, normal));
                A += edge->length() / dL / cosin;
                B += edge->length() / dL * vec[adjTriangle->id()] / cosin;
            }
        }
        result[trInd] = (vec[trInd] * A - B) / triangle->square() + vec[trCount];
        sum = vec[trInd];
    }
    result[trCount] = sum;
    return result;
}
