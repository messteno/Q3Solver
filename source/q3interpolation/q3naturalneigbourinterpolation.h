#ifndef Q3NATURALNEIGBOURINTERPOLATION_H
#define Q3NATURALNEIGBOURINTERPOLATION_H

#include "q3interpolation.h"

extern "C" {
    #include "nn.h"
}

class Q3NaturalNeigbourInterpolation : public Q3Interpolation
{
public:
    Q3NaturalNeigbourInterpolation(QVector<QVector3D> &values);
    virtual ~Q3NaturalNeigbourInterpolation();
    virtual qreal interpolateToPoint(const QPointF &pt);

private:
    QVector<point> points_;
    nnpi *interpolator_;
    delaunay *delaunay_;
};

#endif // Q3NATURALNEIBOURINTERPOLATION_H
