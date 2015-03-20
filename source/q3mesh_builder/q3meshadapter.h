#ifndef Q3MESHADAPTER_H
#define Q3MESHADAPTER_H

#include <QList>
#include <QMap>

#include "q3mesh.h"
#include "q3ani2d.h"
#include "q3sceletonitem.h"

class Q3MeshAdapter
{
public:
    enum SizePolicy
    {
        ElementSizeAuto,
        ElementSizeByCount,
        ElementSizeBySize,
    };

    Q3MeshAdapter();

    virtual bool generateMesh(QList<Q3SceletonItem *> &items,
                              QList<Q3SceletonItem *> &outerBoundary,
                              QList<QList<Q3SceletonItem *> > &innerBoundaries,
                              QList<Q3SceletonItem *> &innerItems) = 0;
    virtual bool meshToQ3Mesh(Q3Mesh *mesh) = 0;

    virtual void setSizePolicy(const SizePolicy &sizePolicy);
    virtual void setElementsCount(int elementsCount);
    virtual void setElementSize(qreal elementSize);

protected:
    qreal elementSize_;
    int elementsCount_;
    SizePolicy sizePolicy_;
    bool created_;
};

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
    bool generateMesh(QList<Q3SceletonItem *> &items,
                      QList<Q3SceletonItem *> &outerBoundary,
                      QList<QList<Q3SceletonItem *> > &innerBoundaries,
                      QList<Q3SceletonItem *> &innerItems);
    bool meshToQ3Mesh(Q3Mesh *mesh);
private:
    Q3Ani2D q3ani2d_;
    QMap<Q3Point*, int> pointMap_;

    void addBoundary(QList<Q3SceletonItem *> &boundary, qreal &square,
                     bool outer, int label);
};

#endif // Q3MESHADAPTER_H
