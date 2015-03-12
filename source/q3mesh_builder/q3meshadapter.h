#ifndef Q3MESHADAPTER_H
#define Q3MESHADAPTER_H

#include <QList>
#include <QMap>

#include "q3ani2d.h"
#include "q3sceletonitem.h"

class Q3MeshAdapter
{
public:
    virtual bool generateMesh(QList<Q3SceletonItem *> &items,
                              QList<Q3SceletonItem *> &outerBoundary,
                              QList<QList<Q3SceletonItem *> > &innerBoundaries,
                              QList<Q3SceletonItem *> innerItems) = 0;
};

class Q3Ani2DMeshAdapter : public Q3MeshAdapter
{
private:
    Q3Ani2D q3ani2d_;
    QMap<Q3Point*, int> pointMap_;
    void addBoundary(QList<Q3SceletonItem *> &boundary, bool outer, int label);
public:
    bool generateMesh(QList<Q3SceletonItem *> &items,
                      QList<Q3SceletonItem *> &outerBoundary,
                      QList<QList<Q3SceletonItem *> > &innerBoundaries,
                      QList<Q3SceletonItem *> innerItems);
    static void circleBoundary(int *param, double *t, double *x, double *y);
};

#endif // Q3MESHADAPTER_H
