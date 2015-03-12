#ifndef Q3ANI2D_H
#define Q3ANI2D_H

#include <string>

#include "ani2D.h"

typedef double real;

class Q3Ani2D
{
public:
    static const double memFactor_;
    static const int maxElements_;
    static const double quality_;

    Q3Ani2D();
    virtual ~Q3Ani2D();
    void reset();
    void setMaxElements(int maxElements);
    void setQuality(double quality);
    void setMaxIters(int iters);

    int addVertex(double x, double y);
    void addEdge(int v0, int v1, int label, int domain, int slitDomain = 0);
    void addCurveEdge(int v0, int v1, double t0, double t1,
                      int label, int curveId, int domain, int slitDomain = 0);

    bool genMeshAnalytic(double (*sizeFunc)(double *),
                         void (*boundaryFunc) (int *, double *, double *, double *));
    bool genMeshFront();
    void save(const std::string& aniFile, const std::string& psFile);
	ani2D* getAni2D ();

private:
    ani2D ani_;
    double *vertices_;
    int *edges_;
    double *curveEdges_;

    int verticesCount_;
    int edgesCount_;
};

#endif // Q3ANI2D_H
