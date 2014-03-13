#include <QDebug>
#include <math.h>
#include "ani2dmeshgenerator.h"

Ani2DMeshGenerator::Ani2DMeshGenerator()
{
}

Ani2DMeshGenerator::~Ani2DMeshGenerator()
{

}

void boundary(int *i, double *t, double *x, double *y)
{
    double s = t[0];
    *x = 0.5 + cos(2. * M_PI * s) * 0.3;
    *y = 0.5 + sin(2. * M_PI * s) * 0.3;
}

Mesh *Ani2DMeshGenerator::generateMesh(const QList<QMeshItem *>& /* items */)
{
    qAni_.reset();

    qAni_.setMaxElements(150000);
    qAni_.setQuality(0.6);
    qAni_.setMaxIters(300);

    qAni_.addVertex(0, 0);
    qAni_.addVertex(0, 1);
    qAni_.addVertex(1, 1);
    qAni_.addVertex(1, 0);
    qAni_.addVertex(0.8, 0.5);
    qAni_.addVertex(0.2, 0.8);

    qAni_.addEdge(0, 1, 1, 1);
    qAni_.addEdge(1, 2, 1, 1);
    qAni_.addEdge(2, 3, 1, 1);
    qAni_.addEdge(3, 0, 1, 1);

    qAni_.addCurveEdge(4, 5, 0, 0.5, 2, 1, 1);
    qAni_.addCurveEdge(5, 4, 0.5, 1, 2, 1, 1);

    qAni_.genMeshAnalytic(NULL, boundary);
    qAni_.save("../out.ani");

    return NULL;
}
