#ifndef Q3NATURALNEIGBOURINTERPOLATION_H
#define Q3NATURALNEIGBOURINTERPOLATION_H

#include "q3meshtrinodeinterpolation.h"

extern "C" {
    #include "nn.h"
}

class Q3NaturalNeigbourInterpolation : public Q3MeshTriNodeInterpolation
{
public:
    Q3NaturalNeigbourInterpolation(Q3Mesh &mesh, QVector<qreal>& triValues);
    virtual ~Q3NaturalNeigbourInterpolation();
    virtual QVector<qreal> interpolateToNodes();
    virtual qreal interpolateToPoint(QPointF *pt);

private:
    nnpi *interpolator_;
    delaunay *delaunay_;
};

#endif // Q3NATURALNEIBOURINTERPOLATION_H
