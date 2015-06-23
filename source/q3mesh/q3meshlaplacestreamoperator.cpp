#include <QDebug>

#include "q3meshlaplacestreamoperator.h"

Q3MeshLaplaceStreamOperator::Q3MeshLaplaceStreamOperator(Q3Mesh &mesh, int size) :
    LinearOperator(size),
    mesh_(mesh)
{

}

Q3Vector Q3MeshLaplaceStreamOperator::operator *(const Q3Vector &vec) const
{
    Q_ASSERT(size_ == vec.size());

    Q3Vector result(size_);

    for (int trInd = 0; trInd < mesh_.triangles().count(); ++trInd)
    {
        Q3MeshTriangle *triangle = mesh_.triangles().at(trInd);
        qreal A = 0;
        qreal B = 0;

        bool noslip = false;

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
            else
            {
                Q3BoundaryType::Type bndType = edge->boundary()->type()->toEnum();

                // TODO: добавить как условие на границу
//                if (bndType == Q3BoundaryType::NoSlipBoundary
//                    || bndType == Q3BoundaryType::FixedVelocity)
//                {
//                    noslip = true;
//                    break;
//                }
                if (qAbs(edge->a()->y()) < 1e-10)
                {
                    noslip = true;
                    break;
                }

                qreal deltaB = edge->processBoundaryStream();
                B += deltaB;
            }
        }

        if (noslip)
            result[trInd] = vec[trInd];
        else
            result[trInd] = (vec[trInd] * A - B) / triangle->square();
    }
    return result;
}

