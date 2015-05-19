#ifndef Q3MESHTRINODEINTERPOLATION_H
#define Q3MESHTRINODEINTERPOLATION_H

#include <QVector>

#include "q3mesh.h"

class Q3MeshTriNodeInterpolation
{
public:
    Q3MeshTriNodeInterpolation(Q3Mesh &mesh, QVector<qreal> triValues);
    virtual QVector<qreal> interpolateToNodes() = 0;
    virtual qreal interpolateToPoint(QPointF *pt) = 0;

protected:
    Q3Mesh &mesh_;
    QVector<qreal> triValues_;
};

#endif // Q3MESHTRINODEINTERPOLATION_H
