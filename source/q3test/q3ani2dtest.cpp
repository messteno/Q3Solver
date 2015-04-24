#include "q3ani2dtest.h"
#include "q3ani2d.h"

Q3Ani2DTest::Q3Ani2DTest()
{

}

Q3Ani2DTest::~Q3Ani2DTest()
{

}

void Q3Ani2DTest::boundary (int *param, double *t, double *x, double *y)
{
    qDebug() << *param << *x << *y;
    *x = 0.5 - 0.25 * cos(*t * 2 * M_PI);
    *y = 0.5 + 0.25 * sin(*t * 2 * M_PI);
}

void Q3Ani2DTest::ani2DCreateMesh()
{
    Q3Ani2D qAni;
    qAni.reset();
    qAni.setMaxElements(1500000);
    qAni.setQuality(0.9);
    qAni.setMaxIters(30000);

    qAni.addVertex(0.25, 0.5);
    qAni.addVertex(0.5, 0.75);
    qAni.addVertex(0.75, 0.5);
    qAni.addVertex(0.5, 0.25);

    qAni.addCurveEdge(0, 1, 0, 0.25, 1, 1, 1, 0);
    qAni.addCurveEdge(1, 2, 0.25, 0.5, 1, 1, 1, 0);
    qAni.addCurveEdge(2, 3, 0.5, 0.75, 1, 1, 1, 0);
    qAni.addCurveEdge(3, 0, 0.75, 1, 1, 1, 1, 0);

    qAni.genMeshAnalytic(NULL, Q3Ani2DTest::boundary, 0.01);

    qAni.save("out.ani");
}

QTEST_APPLESS_MAIN(Q3Ani2DTest)

#include "q3ani2dtest.moc"
