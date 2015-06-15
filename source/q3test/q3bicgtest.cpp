#include "q3bicgtest.h"

Q3BiCGTest::Q3BiCGTest()
{

}

Q3BiCGTest::~Q3BiCGTest()
{

}

void Q3BiCGTest::solveTest()
{
    TestLO lo(2);
    Q3Vector b;
    b << 3 << 7;

    Q3Vector x;
    x << 0 << 0;

    IdentityPreconditioner preconditioner(2);
    BiCGStabLinearSolver solver;
    qreal res = 0;
    solver.solve(lo, x, b, preconditioner, res);
}

QTEST_APPLESS_MAIN(Q3BiCGTest)

#include "q3bicgtest.moc"
