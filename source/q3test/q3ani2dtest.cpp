#include "q3ani2dtest.h"
#include "q3ani2d.h"

Q3Ani2DTest::Q3Ani2DTest()
{

}

Q3Ani2DTest::~Q3Ani2DTest()
{

}

void Q3Ani2DTest::ani2DCreateMesh()
{
    Q3Ani2D qAni;
    qAni.reset();
    qAni.setMaxElements(1500000);
    qAni.setQuality(0.9);
    qAni.setMaxIters(30000);

    qAni.addVertex(0, 0);
    qAni.addVertex(0, 1);
    qAni.addVertex(1, 1);
    qAni.addVertex(1, 0);
    qAni.addVertex(0.25, 0.25);
    qAni.addVertex(0.75, 0.75);

    qAni.addEdge(0, 1, 1, 1);
    qAni.addEdge(1, 2, 1, 1);
    qAni.addEdge(2, 3, 1, 1);
    qAni.addEdge(3, 0, 1, 1);
    qAni.addEdge(4, 3, 1, 1);
    qAni.addEdge(3, 5, 1, 1);
    qAni.addEdge(5, 1, 1, 1);
    qAni.addEdge(1, 4, 1, 1);

    qAni.genMeshAnalytic(NULL, NULL);

    qAni.save ("out.ani", "out.ps");
}

QTEST_APPLESS_MAIN(Q3Ani2DTest)

#include "q3ani2dtest.moc"
