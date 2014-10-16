#include <QDebug>
#include <cstring>
#include "qani2d.h"

const qreal QAni2D::memFactor_ = 1.5;
const int QAni2D::maxElements_ = 1500;
const qreal QAni2D::quality_ = 0.7;

extern "C" {

    int aft2dboundary_( int *pnVert, double *bv,
            int *pnLine, int *bl, double *bltail, double *hsze,
            int *pnVRT, double *vrt,
            int *pnTRI, int *tri, int *labtri,
            int *pnBND, int *bnd, int *labbnd,
            int *pnCRV, double *crv, int *iFNC );

    typedef void (*userfn_t) (int *, double *, double *, double *);
    typedef double (*sizefn_t) (double *);

    extern userfn_t userfn;
    extern sizefn_t sizefn;

}

QAni2D::QAni2D()
{
    memset (&ani_, 0, sizeof(ani2D));
    ani_.control = (f77i *) malloc(6 * sizeof(f77i));
    ani_.control[ANI2D_CONTROL_flagAuto] = 1;
    ani_.control[ANI2D_CONTROL_status]   = 0;
    ani_.control[ANI2D_CONTROL_MaxSkipE] = C2Fmax(200,  maxElements_ / 10);
    ani_.control[ANI2D_CONTROL_MaxQItr]  = C2Fmax(1000, maxElements_ * 10);
    ani_.control[ANI2D_CONTROL_iPrint]   = 1;
    ani_.control[ANI2D_CONTROL_iErrMesg] = 1;

    vertices_ = NULL;
    edges_ = NULL;
    curveEdges_ = NULL;

    setMaxElements(maxElements_);
    setQuality(quality_);

    verticesCount_ = 0;
    edgesCount_ = 0;
}

QAni2D::~QAni2D()
{
    free(ani_.vrt);
    free(ani_.fixedV);
    free(ani_.labelV);

    free(ani_.bnd);
    free(ani_.crv);
    free(ani_.labelB);
    free(ani_.labelC);

    free(ani_.tri);
    free(ani_.fixedT);
    free(ani_.labelT);

    free(ani_.control);

    free(vertices_);
    free(edges_);
    free(curveEdges_);
}

void QAni2D::reset()
{
    ani_.nv = 0;
    ani_.nb = 0;
    ani_.nc = 0;
    ani_.nt = 0;
    ani_.nvfix = 0;
    ani_.nbfix = 0;
    ani_.ntfix = 0;

    verticesCount_ = 0;
    edgesCount_ = 0;
    curveEdges_ = 0;
}

void QAni2D::setMaxElements(int maxElements)
{
    int aniMaxElements = maxElements * memFactor_;

    if (aniMaxElements < ani_.ntmax ||
        aniMaxElements < ani_.nbmax * 4 ||
        aniMaxElements < ani_.nvmax * 2)
    {
        return;
    }
    ani_.nEStar = maxElements;
    ani_.control[ANI2D_CONTROL_MaxSkipE] = C2Fmax(200,  maxElements_ / 10);
    ani_.control[ANI2D_CONTROL_MaxQItr]  = C2Fmax(1000, maxElements_ * 10);

    ani_.nvmax = aniMaxElements / 2;
    ani_.vrt = (f77r *) realloc(ani_.vrt, 2 * ani_.nvmax * sizeof(f77r));
    ani_.labelV = (f77i *) realloc(ani_.labelV, ani_.nvmax * sizeof(f77i));
    ani_.fixedV = (f77i *) realloc(ani_.fixedV, ani_.nvmax * sizeof(f77i));
    vertices_ = (double *) realloc(vertices_, 2 * ani_.nvmax * sizeof(double));

    ani_.nbmax = aniMaxElements / 4;
    ani_.bnd = (f77i *) realloc(ani_.bnd, 4 * ani_.nbmax * sizeof(f77i));
    ani_.crv = (f77r *) realloc(ani_.crv, 2 * ani_.nbmax * sizeof(f77r));
    ani_.fixedB = (f77i *) realloc(ani_.fixedB, ani_.nbmax * sizeof(f77i));
    ani_.labelB = (f77i *) realloc(ani_.labelB, ani_.nbmax * sizeof(f77i));
    ani_.labelC = (f77i *) realloc(ani_.labelC, ani_.nbmax * sizeof(f77i));
    edges_ = (int *) realloc(edges_, 7 * ani_.nbmax * sizeof(int));
    curveEdges_ = (double *) realloc(curveEdges_, 2 * ani_.nbmax * sizeof(double));

    ani_.ntmax = aniMaxElements;
    ani_.tri = (f77i *) realloc(ani_.tri, 3 * ani_.ntmax * sizeof(f77i));
    ani_.labelT = (f77i *) realloc(ani_.labelT, ani_.ntmax * sizeof(f77i));
    ani_.fixedT = (f77i *) realloc(ani_.fixedT, ani_.ntmax * sizeof(f77i));
}

void QAni2D::setQuality(qreal quality)
{
    ani_.Quality = quality;
}

void QAni2D::setMaxIters(int iters)
{
    ani_.control[ANI2D_CONTROL_MaxQItr] = iters;
}

void QAni2D::addVertex(double x, double y)
{
    if (verticesCount_ > ani_.nvmax)
        return;
    vertices_[2 * verticesCount_ + 0] = x;
    vertices_[2 * verticesCount_ + 1] = y;

    verticesCount_++;
}

void QAni2D::addEdge(int v0, int v1, int label, int domain, int slitDomain)
{
    if (v0 >= verticesCount_ || v1 >= verticesCount_)
        return;
    edges_[7 * edgesCount_ + 0] = v0 + 1;
    edges_[7 * edgesCount_ + 1] = v1 + 1;
    edges_[7 * edgesCount_ + 2] = 0;
    edges_[7 * edgesCount_ + 3] = -1;
    edges_[7 * edgesCount_ + 4] = label;
    edges_[7 * edgesCount_ + 5] = domain;
    edges_[7 * edgesCount_ + 6] = slitDomain;

    curveEdges_[2 * edgesCount_ + 0] = 0;
    curveEdges_[2 * edgesCount_ + 1] = 0;

    edgesCount_++;
}

void QAni2D::addCurveEdge(int v0, int v1, double t0, double t1,
                          int label, int curveId, int domain, int slitDomain)
{
    if (v0 >= verticesCount_ || v1 >= verticesCount_)
        return;
    edges_[7 * edgesCount_ + 0] = v0 + 1;
    edges_[7 * edgesCount_ + 1] = v1 + 1;
    edges_[7 * edgesCount_ + 2] = curveId;
    edges_[7 * edgesCount_ + 3] = -1;
    edges_[7 * edgesCount_ + 4] = label;
    edges_[7 * edgesCount_ + 5] = domain;
    edges_[7 * edgesCount_ + 6] = slitDomain;

    curveEdges_[2 * edgesCount_ + 0] = t0;
    curveEdges_[2 * edgesCount_ + 1] = t1;

    edgesCount_++;
}

void QAni2D::genMeshAnalytic(double (*sizeFunc)(double *),
                             void (*boundaryFunc)(int *, double *, double *, double *))
{
    double h = 1e-2;

    if (sizeFunc)
        sizefn = (sizefn_t) sizeFunc;

    if (boundaryFunc)
        userfn = (userfn_t) boundaryFunc;

    aft2dboundary_(&verticesCount_, vertices_, &edgesCount_, edges_, curveEdges_, &h,
                   &ani_.nv, ani_.vrt, &ani_.nt, ani_.tri, ani_.labelT,
                   &ani_.nb, ani_.bnd, ani_.labelB, &ani_.nc, ani_.crv, ani_.labelC);
}

void QAni2D::save(const QString &aniFile, const QString &psFile)
{
    ani2D_save_mesh(&ani_, aniFile.toUtf8().data());
    ani2D_draw_mesh(&ani_, psFile.toUtf8().data());
}

