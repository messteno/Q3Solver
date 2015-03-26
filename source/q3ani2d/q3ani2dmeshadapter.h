#ifndef Q3ANI2DMESHADAPTER_H
#define Q3ANI2DMESHADAPTER_H

#include "q3meshadapter.h"
#include "q3ani2d.h"

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
    bool generateMesh(Q3Sceleton *sceleton);
    bool meshToQ3Mesh(Q3Mesh *mesh);
private:
    Q3Ani2D q3ani2d_;
    QMap<Q3Point*, int> pointMap_;

    void addBoundary(QList<Q3SceletonItem *> &boundary, qreal &square,
                     bool outer, int label);
};

#endif // Q3ANI2DMESHADAPTER_H

