#include <float.h>

#include "interpolation.h"

Interpolation::Interpolation(QVector<QVector3D> &values) :
    values_(values)
{
}

Interpolation::~Interpolation()
{
}
