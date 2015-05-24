#include "q3testbuilder.h"
#include "q3boundaryin.h"
#include "q3boundaryfixedvelocity.h"
#include "q3ani2dmeshadapter.h"

void Q3CavityTestBuilder::buildTest(Q3Sceleton &sceleton,
                                    Q3Mesh &mesh,
                                    QList<Q3Boundary *> &boundaries)
{
    Q3Point *a1 = new Q3Point(QPointF(0, 0));
    Q3Point *a2 = new Q3Point(QPointF(0, 1));
    Q3Point *a3 = new Q3Point(QPointF(1, 1));
    Q3Point *a4 = new Q3Point(QPointF(1, 0));
    sceleton.addItem(a1);
    sceleton.addItem(a2);
    sceleton.addItem(a3);
    sceleton.addItem(a4);
    Q3PointConnection *c1 = new Q3PointConnection(a1, a2);
    Q3PointConnection *c2 = new Q3PointConnection(a2, a3);
    Q3PointConnection *c3 = new Q3PointConnection(a3, a4);
    Q3PointConnection *c4 = new Q3PointConnection(a4, a1);
    sceleton.addItem(c1);
    sceleton.addItem(c2);
    sceleton.addItem(c3);
    sceleton.addItem(c4);

    Q3Boundary *b1 = new Q3Boundary();
    Q3Boundary *b2 = new Q3Boundary();
    Q3Boundary *b3 = new Q3Boundary();
    Q3Boundary *b4 = new Q3Boundary();
    b1->addItem(c1);
    b1->setTypeByEnum(Q3BoundaryType::NoSlipBoundary);
    b2->addItem(c2);
    b2->setTypeByEnum(Q3BoundaryType::FixedVelocity);
    Q3BoundaryFixedVelocity *fixedVelocityType = \
            dynamic_cast<Q3BoundaryFixedVelocity *>(b2->type());
    fixedVelocityType->setVelocityText("1");
    b3->addItem(c3);
    b3->setTypeByEnum(Q3BoundaryType::NoSlipBoundary);
    b4->addItem(c4);
    b4->setTypeByEnum(Q3BoundaryType::NoSlipBoundary);
    boundaries.append(b1);
    boundaries.append(b2);
    boundaries.append(b3);
    boundaries.append(b4);

    sceleton.prepare();

    Q3Boundary::setUniqueLabels(&boundaries);
    Q3MeshAdapter *adapter = new Q3Ani2DMeshAdapter();
    adapter->setSizePolicy(Q3MeshAdapter::ElementSizeByCount);
    adapter->setElementsCount(15000);
    adapter->generateMesh(&sceleton, &boundaries);
    adapter->meshToQ3Mesh(&mesh, &boundaries);
    delete adapter;
}

void Q3FlowPastCylinderTestBuilder::buildTest(Q3Sceleton &sceleton,
                                              Q3Mesh &mesh,
                                              QList<Q3Boundary *> &boundaries)
{
    Q3Point *a1 = new Q3Point(QPointF(0, 0));
    Q3Point *a2 = new Q3Point(QPointF(0, 4.1));
    Q3Point *a3 = new Q3Point(QPointF(22, 4.1));
    Q3Point *a4 = new Q3Point(QPointF(22, 0));
    sceleton.addItem(a1);
    sceleton.addItem(a2);
    sceleton.addItem(a3);
    sceleton.addItem(a4);
    Q3PointConnection *c1 = new Q3PointConnection(a1, a2);
    Q3PointConnection *c2 = new Q3PointConnection(a2, a3);
    Q3PointConnection *c3 = new Q3PointConnection(a3, a4);
    Q3PointConnection *c4 = new Q3PointConnection(a4, a1);
    sceleton.addItem(c1);
    sceleton.addItem(c2);
    sceleton.addItem(c3);
    sceleton.addItem(c4);
    Q3Circle *circle = new Q3Circle(QPointF(2, 2), 0.5);
    sceleton.addItem(circle);

    Q3Boundary *b1 = new Q3Boundary();
    Q3Boundary *b2 = new Q3Boundary();
    Q3Boundary *b3 = new Q3Boundary();
    Q3Boundary *b4 = new Q3Boundary();
    Q3Boundary *b5 = new Q3Boundary();
    b1->addItem(c1);
    b1->setTypeByEnum(Q3BoundaryType::InBoundary);
    Q3BoundaryIn *boundaryIn = dynamic_cast<Q3BoundaryIn *>(b1->type());
    boundaryIn->setVelocityText("y * (4.1 - y) / 4.1 / 4.1 * 4", "0");
    b2->addItem(c2);
    b2->setTypeByEnum(Q3BoundaryType::NoSlipBoundary);
    b3->addItem(c3);
    b3->setTypeByEnum(Q3BoundaryType::OutBoundary);
    b4->addItem(c4);
    b4->setTypeByEnum(Q3BoundaryType::NoSlipBoundary);
    b5->addItem(circle);
    b5->setTypeByEnum(Q3BoundaryType::NoSlipBoundary);
    boundaries.append(b1);
    boundaries.append(b2);
    boundaries.append(b3);
    boundaries.append(b4);
    boundaries.append(b5);

    sceleton.prepare();

    Q3Boundary::setUniqueLabels(&boundaries);
    Q3MeshAdapter *adapter = new Q3Ani2DMeshAdapter();
    adapter->setSizePolicy(Q3MeshAdapter::ElementSizeByCount);
    adapter->setElementsCount(15000);
    adapter->generateMesh(&sceleton, &boundaries);
    adapter->meshToQ3Mesh(&mesh, &boundaries);
    delete adapter;
}
