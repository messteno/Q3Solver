#include "q3testbuilder.h"
#include "q3boundaryin.h"
#include "q3boundaryfixedvelocity.h"
#include "q3ani2dmeshadapter.h"

void Q3CavityTestBuilder::buildTest(Q3Sceleton &sceleton,
                                    Q3Mesh &mesh,
                                    QList<Q3Boundary *> &boundaries,
                                    Q3MeshAdapter &adapter)
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

    Q3Boundary::setUniqueLabels(boundaries);
    adapter.setSizePolicy(Q3MeshAdapter::ElementSizeByCount);
    adapter.setElementsCount(20000);
    adapter.generateMesh(sceleton, boundaries);
    adapter.meshToQ3Mesh(mesh, boundaries);
}

void Q3FlowPastCylinderTestBuilder::buildTest(Q3Sceleton &sceleton,
                                              Q3Mesh &mesh,
                                              QList<Q3Boundary *> &boundaries,
                                              Q3MeshAdapter &adapter)
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

    Q3Boundary::setUniqueLabels(boundaries);
    adapter.setSizePolicy(Q3MeshAdapter::ElementSizeByCount);
    adapter.setElementsCount(15000);
    adapter.generateMesh(sceleton, boundaries);
    adapter.meshToQ3Mesh(mesh, boundaries);
}

void Q3PoiseuilleFlowTestBuilder::buildTest(Q3Sceleton &sceleton,
                                            Q3Mesh &mesh,
                                            QList<Q3Boundary *> &boundaries,
                                            Q3MeshAdapter &adapter)
{
    Q3Point *a1 = new Q3Point(QPointF(0, 0));
    Q3Point *a2 = new Q3Point(QPointF(0, 1));
    Q3Point *a3 = new Q3Point(QPointF(30, 1));
    Q3Point *a4 = new Q3Point(QPointF(30, 0));
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
    b1->setTypeByEnum(Q3BoundaryType::InBoundary);
    Q3BoundaryIn *boundaryIn = dynamic_cast<Q3BoundaryIn *>(b1->type());
    boundaryIn->setVelocityText("1", "0");
    b2->addItem(c2);
    b2->setTypeByEnum(Q3BoundaryType::NoSlipBoundary);
    b3->addItem(c3);
    b3->setTypeByEnum(Q3BoundaryType::OutBoundary);
    b4->addItem(c4);
    b4->setTypeByEnum(Q3BoundaryType::NoSlipBoundary);
    boundaries.append(b1);
    boundaries.append(b2);
    boundaries.append(b3);
    boundaries.append(b4);

    sceleton.prepare();

    Q3Boundary::setUniqueLabels(boundaries);
    adapter.setSizePolicy(Q3MeshAdapter::ElementSizeByCount);
    adapter.setElementsCount(15000);
    adapter.generateMesh(sceleton, boundaries);
    adapter.meshToQ3Mesh(mesh, boundaries);
}

void Q3FlowPastCircularCylinderTestBuilder::buildTest(Q3Sceleton &sceleton,
                                                      Q3Mesh &mesh,
                                                      QList<Q3Boundary *> &boundaries,
                                                      Q3MeshAdapter &adapter)
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
    b5->setTypeByEnum(Q3BoundaryType::FixedVelocity);
    Q3BoundaryFixedVelocity *circleBoundary =
            dynamic_cast<Q3BoundaryFixedVelocity *>(b5->type());
    circleBoundary->setVelocityText("1");
    boundaries.append(b1);
    boundaries.append(b2);
    boundaries.append(b3);
    boundaries.append(b4);
    boundaries.append(b5);

    sceleton.prepare();

    Q3Boundary::setUniqueLabels(boundaries);
    adapter.setSizePolicy(Q3MeshAdapter::ElementSizeByCount);
    adapter.setElementsCount(15000);
    adapter.generateMesh(sceleton, boundaries);
    adapter.meshToQ3Mesh(mesh, boundaries);
}

void Q3BackwardFacingStepTestBuilder::buildTest(Q3Sceleton &sceleton,
                                                Q3Mesh &mesh,
                                                QList<Q3Boundary *> &boundaries,
                                                Q3MeshAdapter &adapter)
{

    Q3Point *a1 = new Q3Point(QPointF(0, 0.5));
    Q3Point *a2 = new Q3Point(QPointF(0, 1));
    Q3Point *a3 = new Q3Point(QPointF(15, 1));
    Q3Point *a4 = new Q3Point(QPointF(15, 0));
    Q3Point *a5 = new Q3Point(QPointF(0, 0));
    sceleton.addItem(a1);
    sceleton.addItem(a2);
    sceleton.addItem(a3);
    sceleton.addItem(a4);
    sceleton.addItem(a5);
    Q3PointConnection *c1 = new Q3PointConnection(a1, a2);
    Q3PointConnection *c2 = new Q3PointConnection(a2, a3);
    Q3PointConnection *c3 = new Q3PointConnection(a3, a4);
    Q3PointConnection *c4 = new Q3PointConnection(a4, a5);
    Q3PointConnection *c5 = new Q3PointConnection(a5, a1);
    sceleton.addItem(c1);
    sceleton.addItem(c2);
    sceleton.addItem(c3);
    sceleton.addItem(c4);
    sceleton.addItem(c5);

    Q3Boundary *b1 = new Q3Boundary();
    Q3Boundary *b2 = new Q3Boundary();
    Q3Boundary *b3 = new Q3Boundary();
    Q3Boundary *b4 = new Q3Boundary();
    Q3Boundary *b5 = new Q3Boundary();
    b1->addItem(c1);
    b1->setTypeByEnum(Q3BoundaryType::InBoundary);
    Q3BoundaryIn *boundaryIn = dynamic_cast<Q3BoundaryIn *>(b1->type());
    boundaryIn->setVelocityText("24*(1-y)*(y-0.5)", "0");
    b2->addItem(c2);
    b2->setTypeByEnum(Q3BoundaryType::NoSlipBoundary);
    b3->addItem(c3);
    b3->setTypeByEnum(Q3BoundaryType::OutBoundary);
    b4->addItem(c4);
    b4->setTypeByEnum(Q3BoundaryType::NoSlipBoundary);
    b5->addItem(c5);
    b5->setTypeByEnum(Q3BoundaryType::NoSlipBoundary);
    boundaries.append(b1);
    boundaries.append(b2);
    boundaries.append(b3);
    boundaries.append(b4);
    boundaries.append(b5);

    sceleton.prepare();

    Q3Boundary::setUniqueLabels(boundaries);
    adapter.setSizePolicy(Q3MeshAdapter::ElementSizeByCount);
    adapter.setElementsCount(15000);
    adapter.generateMesh(sceleton, boundaries);
    adapter.meshToQ3Mesh(mesh, boundaries);
}

void Q3FlowPastSquareTestBuilder::buildTest(Q3Sceleton &sceleton,
                                            Q3Mesh &mesh,
                                            QList<Q3Boundary *> &boundaries,
                                            Q3MeshAdapter &adapter)
{
    Q3Point *a1 = new Q3Point(QPointF(0, 0));
    Q3Point *a2 = new Q3Point(QPointF(0, 18));
    Q3Point *a3 = new Q3Point(QPointF(26, 18));
    Q3Point *a4 = new Q3Point(QPointF(26, 0));
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

    Q3Point *a5 = new Q3Point(QPointF(8.5, 8.5));
    Q3Point *a6 = new Q3Point(QPointF(8.5, 9.5));
    Q3Point *a7 = new Q3Point(QPointF(9.5, 9.5));
    Q3Point *a8 = new Q3Point(QPointF(9.5, 8.5));
    sceleton.addItem(a5);
    sceleton.addItem(a6);
    sceleton.addItem(a7);
    sceleton.addItem(a8);
    Q3PointConnection *c5 = new Q3PointConnection(a5, a6);
    Q3PointConnection *c6 = new Q3PointConnection(a6, a7);
    Q3PointConnection *c7 = new Q3PointConnection(a7, a8);
    Q3PointConnection *c8 = new Q3PointConnection(a8, a5);
    sceleton.addItem(c5);
    sceleton.addItem(c6);
    sceleton.addItem(c7);
    sceleton.addItem(c8);

    Q3Boundary *b1 = new Q3Boundary();
    Q3Boundary *b2 = new Q3Boundary();
    Q3Boundary *b3 = new Q3Boundary();
    Q3Boundary *b4 = new Q3Boundary();
    Q3Boundary *b5 = new Q3Boundary();
    Q3Boundary *b6 = new Q3Boundary();
    Q3Boundary *b7 = new Q3Boundary();
    Q3Boundary *b8 = new Q3Boundary();
    b1->addItem(c1);
    b1->setTypeByEnum(Q3BoundaryType::InBoundary);
    Q3BoundaryIn *boundaryIn = dynamic_cast<Q3BoundaryIn *>(b1->type());
    boundaryIn->setVelocityText("1", "0");
    b2->addItem(c2);
    b2->setTypeByEnum(Q3BoundaryType::NoSlipBoundary);
    b3->addItem(c3);
    b3->setTypeByEnum(Q3BoundaryType::OutBoundary);
    b4->addItem(c4);
    b4->setTypeByEnum(Q3BoundaryType::NoSlipBoundary);
    b5->addItem(c5);
    b5->setTypeByEnum(Q3BoundaryType::NoSlipBoundary);
    b6->addItem(c6);
    b6->setTypeByEnum(Q3BoundaryType::NoSlipBoundary);
    b7->addItem(c7);
    b7->setTypeByEnum(Q3BoundaryType::NoSlipBoundary);
    b8->addItem(c8);
    b8->setTypeByEnum(Q3BoundaryType::NoSlipBoundary);
    boundaries.append(b1);
    boundaries.append(b2);
    boundaries.append(b3);
    boundaries.append(b4);
    boundaries.append(b5);
    boundaries.append(b6);
    boundaries.append(b7);
    boundaries.append(b8);

    sceleton.prepare();

    Q3Boundary::setUniqueLabels(boundaries);
    adapter.setSizePolicy(Q3MeshAdapter::ElementSizeByCount);
    adapter.setElementsCount(15000);
    adapter.generateMesh(sceleton, boundaries);
    adapter.meshToQ3Mesh(mesh, boundaries);
}
