#ifndef INTERPOLATION_H
#define INTERPOLATION_H

#include <QVector>
#include <QVector3D>

#include "q3mesh.h"

class Interpolation
{
public:
    Interpolation(QVector<QVector3D> &values);
    virtual ~Interpolation();
    virtual qreal interpolateToPoint(const QPointF &pt) = 0;

protected:
    QVector<QVector3D> &values_;
};

#endif // INTERPOLATION_H
