#ifndef Q3TESTBUILDER_H
#define Q3TESTBUILDER_H

#include "q3mesh.h"
#include "q3sceleton.h"
#include "q3boundary.h"

class Q3TestBuilder
{
public:
    virtual void buildTest(Q3Sceleton &sceleton, Q3Mesh &mesh,
                           QList<Q3Boundary *> &boundaries) = 0;
};

class Q3CavityTestBuilder : public Q3TestBuilder
{
public:
    void buildTest(Q3Sceleton &sceleton, Q3Mesh &mesh,
                   QList<Q3Boundary *> &boundaries);
};

class Q3FlowPastCylinderTestBuilder : public Q3TestBuilder
{
public:
    void buildTest(Q3Sceleton &sceleton, Q3Mesh &mesh,
                   QList<Q3Boundary *> &boundaries);
};

class Q3PoiseuilleFlowTestBuilder : public Q3TestBuilder
{
public:
    void buildTest(Q3Sceleton &sceleton, Q3Mesh &mesh,
                   QList<Q3Boundary *> &boundaries);
};

class Q3FlowPastCircularCylinderTestBuilder : public Q3TestBuilder
{
public:
    void buildTest(Q3Sceleton &sceleton, Q3Mesh &mesh,
                   QList<Q3Boundary *> &boundaries);
};

class Q3BackwardFacingStepTestBuilder : public Q3TestBuilder
{
public:
    void buildTest(Q3Sceleton &sceleton, Q3Mesh &mesh,
                   QList<Q3Boundary *> &boundaries);
};

class Q3FlowPastSquareTestBuilder : public Q3TestBuilder
{
public:
    void buildTest(Q3Sceleton &sceleton, Q3Mesh &mesh,
                   QList<Q3Boundary *> &boundaries);
};

#endif // Q3TESTBUILDER_H
