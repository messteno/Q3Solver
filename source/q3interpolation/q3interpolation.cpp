#include <float.h>

#include "q3interpolation.h"

Q3Interpolation::Q3Interpolation(QVector<QVector3D> &values) :
    values_(values)
{
}

Q3Interpolation::~Q3Interpolation()
{
}
