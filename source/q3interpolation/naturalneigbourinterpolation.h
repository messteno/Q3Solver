#ifndef NATURALNEIGBOURINTERPOLATION_H
#define NATURALNEIGBOURINTERPOLATION_H

#include "interpolation.h"

extern "C" {
    #include "nn.h"
}

class NaturalNeigbourInterpolation : public Interpolation
{
public:
    NaturalNeigbourInterpolation(QVector<QVector3D> &values);
    virtual ~NaturalNeigbourInterpolation();
    virtual qreal interpolateToPoint(const QPointF &pt);

private:
    QVector<point> points_;
    nnpi *interpolator_;
    delaunay *delaunay_;
};

#endif // NATURALNEIBOURINTERPOLATION_H
