#ifndef Q3INTERPOLATION_H
#define Q3INTERPOLATION_H

#include <QVector>
#include <QVector3D>

#include "q3mesh.h"

class Q3Interpolation
{
public:
    Q3Interpolation(QVector<QVector3D> &values);
    virtual ~Q3Interpolation();
    virtual qreal interpolateToPoint(const QPointF &pt) = 0;

protected:
    QVector<QVector3D> &values_;
};

#endif // Q3INTERPOLATION_H
