#include <QDebug>
#include <math.h>
#include "qmeshitempoint.h"
#include "qmeshitemline.h"
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

Mesh *Ani2DMeshGenerator::generateMesh(const QList<QMeshItem *>& items)
{
    std::map<QMeshItemPoint *, int> pointsNumMap;
    int pointsCount = 0;

    qAni_.reset();

    qAni_.setMaxElements(150000);
    qAni_.setQuality(0.6);
    qAni_.setMaxIters(300);

    foreach(QMeshItem *item, items)
    {
        QMeshItemLine *line = dynamic_cast<QMeshItemLine *>(item);
        if (line != 0)
        {
            QMeshItemPoint *p1 = line->a();
            QMeshItemPoint *p2 = line->b();

            // QAssert(p1);
            // QAssert(p2);
            
            if (pointsNumMap.find(p1) == pointsNumMap.end())
            {
                pointsNumMap[p1] = pointsCount++;
                qAni_.addVertex(p1->x(), p1->y());
            }

            if (pointsNumMap.find(p2) == pointsNumMap.end())
            {
                pointsNumMap[p2] = pointsCount++;
                qAni_.addVertex(p2->x(), p2->y());
            }
            qAni_.addEdge(pointsNumMap[p1], pointsNumMap[p2], 1, 1);
        }
    }

    qAni_.genMeshAnalytic(NULL, boundary);
    qAni_.save("../out.ani", "../out.ps");

    return NULL;
}

