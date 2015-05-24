#ifndef Q3TESTBUILDER_H
#define Q3TESTBUILDER_H

#include "q3mesh.h"
#include "q3sceleton.h"
#include "q3boundary.h"

class Q3TestBuilder
{
public:
    Q3TestBuilder();
    virtual void buildTest(Q3Sceleton &sceleton, Q3Mesh &mesh,
                           QList<Q3Boundary *> &boundaries) = 0;
};

class Q3CavityTestBuilder : public Q3TestBuilder
{
public:
    Q3CavityTestBuilder();
    void buildTest(Q3Sceleton &sceleton, Q3Mesh &mesh,
                   QList<Q3Boundary *> &boundaries);
};

#endif // Q3TESTBUILDER_H
