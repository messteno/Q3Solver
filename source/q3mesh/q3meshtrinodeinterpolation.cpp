#include <float.h>

#include "q3meshtrinodeinterpolation.h"

Q3MeshTriNodeInterpolation::Q3MeshTriNodeInterpolation(Q3Mesh &mesh,
                                                       QVector<qreal> triValues) :
    mesh_(mesh),
    triValues_(triValues)
{
}
