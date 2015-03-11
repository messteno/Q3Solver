#ifndef Q3MESHADAPTER_H
#define Q3MESHADAPTER_H

#include <QList>

#include "q3sceletonitem.h"

class Q3MeshAdapter
{
public:
    virtual void generateMesh(QList<Q3SceletonItem *> &items,
                              QList<Q3SceletonItem *> &outerBoundary,
                              QList<QList<Q3SceletonItem *> > &innerBoundaries,
                              QList<Q3SceletonItem *> innerItems) = 0;
};

class Q3Ani2DMeshAdapter : public Q3MeshAdapter
{
public:
    void generateMesh(QList<Q3SceletonItem *> &items,
                     QList<Q3SceletonItem *> &outerBoundary,
                     QList<QList<Q3SceletonItem *> > &innerBoundaries,
                     QList<Q3SceletonItem *> innerItems);
};

#endif // Q3MESHADAPTER_H
