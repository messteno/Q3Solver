#ifndef QANI2D_H
#define QANI2D_H

#include <QtGlobal>
#include <QString>
#include "ani2D.h"

class QAni2D
{
public:
    static const qreal memFactor_;
    static const int maxElements_;
    static const qreal quality_;

    QAni2D();
    virtual ~QAni2D();
    void reset();
    void setMaxElements(int maxElements);
    void setQuality(qreal quality);
    void setMaxIters(int iters);

    void addVertex(double x, double y);
    void addEdge(int v0, int v1, int label, int domain, int slitDomain = 0);
    void addCurveEdge(int v0, int v1, double t0, double t1,
                      int label, int curveId, int domain, int slitDomain = 0);

    void genMeshAnalytic(double (*sizeFunc)(double *),
                         void (*boundaryFunc) (int *, double *, double *, double *));
    void save(const QString& file);

private:
    ani2D ani_;
    double *vertices_;
    int *edges_;
    double *curveEdges_;

    int verticesCount_;
    int edgesCount_;
};

#endif // QANI2D_H
