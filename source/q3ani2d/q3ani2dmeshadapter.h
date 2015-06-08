#ifndef Q3ANI2DMESHADAPTER_H
#define Q3ANI2DMESHADAPTER_H

#include <QVector>
#include <QList>

#include "q3meshadapter.h"
#include "q3ani2d.h"
#include "q3boundary.h"

class Q3Ani2DMeshAdapter : public Q3MeshAdapter
{
public:
    typedef struct
    {
        Q3SceletonItem *item;
        bool outer;
    } CurveBoundary;

    Q3Ani2DMeshAdapter();
    static void circleBoundary(int *param, double *t, double *x, double *y);
    bool generateMesh(Q3Sceleton &sceleton, QList<Q3Boundary *> &boundaries);
    bool meshToQ3Mesh(Q3Mesh &mesh, QList<Q3Boundary *> &boundaries);
    bool saveMesh();
private:
    Q3Ani2D q3ani2d_;
    QMap<Q3Point*, int> pointMap_;
    QList<int> labelBoundaryDelimeters_;

    bool addBoundary(QList<Q3Boundary *> &boundaries,
                     QList<Q3SceletonItem *> &boundary, qreal &square,
                     bool outer);

    static double distToBoundary(double *point);
    static double sizeFunction (double *point);

};
#endif // Q3ANI2DMESHADAPTER_H

