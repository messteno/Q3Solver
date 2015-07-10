#ifndef Q3TESTBUILDER_H
#define Q3TESTBUILDER_H

#include "q3mesh.h"
#include "q3sceleton.h"
#include "q3boundary.h"
#include "q3meshadapter.h"

class Q3TestBuilder
{
public:
    virtual void buildTest(Q3Sceleton &sceleton, Q3Mesh &mesh,
                           QList<Q3Boundary *> &boundaries,
                           Q3MeshAdapter &adapter) = 0;
};

class Q3CavityTestBuilder : public Q3TestBuilder
{
public:
    void buildTest(Q3Sceleton &sceleton, Q3Mesh &mesh,
                   QList<Q3Boundary *> &boundaries, Q3MeshAdapter &adapter);
};

class Q3FlowPastCylinderTestBuilder : public Q3TestBuilder
{
public:
    void buildTest(Q3Sceleton &sceleton, Q3Mesh &mesh,
                   QList<Q3Boundary *> &boundaries, Q3MeshAdapter &adapter);
};

class Q3PoiseuilleFlowTestBuilder : public Q3TestBuilder
{
public:
    void buildTest(Q3Sceleton &sceleton, Q3Mesh &mesh,
                   QList<Q3Boundary *> &boundaries, Q3MeshAdapter &adapter);
};

class Q3FlowPastRotatingCylinderTestBuilder : public Q3TestBuilder
{
public:
    void buildTest(Q3Sceleton &sceleton, Q3Mesh &mesh,
                   QList<Q3Boundary *> &boundaries, Q3MeshAdapter &adapter);
};

class Q3BackwardFacingStepTestBuilder : public Q3TestBuilder
{
public:
    void buildTest(Q3Sceleton &sceleton, Q3Mesh &mesh,
                   QList<Q3Boundary *> &boundaries, Q3MeshAdapter &adapter);
};

class Q3FlowPastSquareTestBuilder : public Q3TestBuilder
{
public:
    void buildTest(Q3Sceleton &sceleton, Q3Mesh &mesh,
                   QList<Q3Boundary *> &boundaries, Q3MeshAdapter &adapter);
};

#endif // Q3TESTBUILDER_H
