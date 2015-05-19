#include "q3solver.h"
#include "q3movedirector.h"
#include "q3sceletoneditor.h"
#include "q3boundaryfixedvelocity.h"
#include "q3ani2dmeshadapter.h"
#include "ui_q3solver.h"

Q3Solver::Q3Solver(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::Q3Solver),
    sceletonEditor_(NULL),
    meshEditor_(NULL),
    boundaryEditor_(NULL),
    calculusEditor_(NULL),
    tabWidgetIndex_(-1)
{
    ui->setupUi(this);

    sceleton_ = new Q3Sceleton(this);
    mesh_ = new Q3Mesh(this);

    Q3Point *a1 = new Q3Point(QPointF(0, 0));
    Q3Point *a2 = new Q3Point(QPointF(0, 1));
    Q3Point *a3 = new Q3Point(QPointF(1, 1));
    Q3Point *a4 = new Q3Point(QPointF(1, 0));
    sceleton_->addItem(a1);
    sceleton_->addItem(a2);
    sceleton_->addItem(a3);
    sceleton_->addItem(a4);
    Q3PointConnection *c1 = new Q3PointConnection(a1, a2);
    Q3PointConnection *c2 = new Q3PointConnection(a2, a3);
    Q3PointConnection *c3 = new Q3PointConnection(a3, a4);
    Q3PointConnection *c4 = new Q3PointConnection(a4, a1);
    sceleton_->addItem(c1);
    sceleton_->addItem(c2);
    sceleton_->addItem(c3);
    sceleton_->addItem(c4);
    Q3Boundary *b1 = new Q3Boundary();
    Q3Boundary *b2 = new Q3Boundary();
    Q3Boundary *b3 = new Q3Boundary();
    Q3Boundary *b4 = new Q3Boundary();
    b1->addItem(c1);
    b1->setTypeByEnum(Q3BoundaryType::NoSlipBoundary);
    b2->addItem(c2);
    b2->setTypeByEnum(Q3BoundaryType::FixedVelocity);
    b3->addItem(c3);
    b3->setTypeByEnum(Q3BoundaryType::NoSlipBoundary);
    b4->addItem(c4);
    b4->setTypeByEnum(Q3BoundaryType::NoSlipBoundary);
    boundaries_.append(b1);
    boundaries_.append(b2);
    boundaries_.append(b3);
    boundaries_.append(b4);

    sceleton_->prepare();

    Q3Boundary::setUniqueLabels(&boundaries_);
    Q3MeshAdapter *adapter = new Q3Ani2DMeshAdapter();
    adapter->setSizePolicy(Q3MeshAdapter::ElementSizeByCount);
    adapter->setElementsCount(15000);
    adapter->generateMesh(sceleton_, &boundaries_);
    adapter->meshToQ3Mesh(mesh_, &boundaries_);
//    adapter->saveMesh();
    delete adapter;

    meshEditor_ = new Q3MeshEditor(ui->plotWidget, mesh_, sceleton_,
                                   &boundaries_);
    calculusEditor_ = new Q3CalculusEditor(ui->plotWidget, mesh_);

    ui->meshEditorLayout->addWidget(meshEditor_);
    ui->calculusEditorLayout->addWidget(calculusEditor_);

    on_tabWidget_currentChanged(0);
}

Q3Solver::~Q3Solver()
{
    delete ui;
}

void Q3Solver::paintEvent(QPaintEvent *event)
{
}

Q3Mesh* Q3Solver::mesh() const
{
    return mesh_;
}

Q3Sceleton *Q3Solver::sceleton() const
{
    return sceleton_;
}

void Q3Solver::on_tabWidget_currentChanged(int index)
{
    ui->plotWidget->clearDrawable();

    if (tabWidgetIndex_ == 0)
    {
        delete sceletonEditor_;
        sceletonEditor_ = NULL;
    }

    else if (tabWidgetIndex_ == 1)
    {
        delete boundaryEditor_;
        boundaryEditor_ = NULL;
    }
    else if (tabWidgetIndex_ == 2)
    {
        meshEditor_->disable();
    }
    else if (tabWidgetIndex_ == 3)
    {
        calculusEditor_->disable();
    }

    if (index == 0)
    {
        ui->plotWidget->addDrawable(sceleton_);
        sceletonEditor_ = new Q3SceletonEditor(ui->plotWidget, sceleton_);
        ui->sceletonEditorLayout->addWidget(sceletonEditor_);

        connect(sceletonEditor_, SIGNAL(goToTab(int)),
                ui->tabWidget, SLOT(setCurrentIndex(int)));
    }
    else if (index == 1)
    {
        ui->plotWidget->addDrawable(sceleton_);
        boundaryEditor_ = new Q3BoundaryEditor(ui->plotWidget,
                                               sceleton_, &boundaries_);
        ui->boundaryEditorLayout->addWidget(boundaryEditor_);

        connect(boundaryEditor_, SIGNAL(goToTab(int)),
                ui->tabWidget, SLOT(setCurrentIndex(int)));
    }
    else if (index == 2)
    {
//        ui->plotWidget->addDrawable(sceleton_);
        ui->plotWidget->addDrawable(mesh_);
        meshEditor_->enable();
//        ui->meshEditorLayout->addWidget(meshEditor_);

        connect(meshEditor_, SIGNAL(goToTab(int)),
                ui->tabWidget, SLOT(setCurrentIndex(int)));
    }
    else if (index == 3)
    {
//        ui->plotWidget->addDrawable(sceleton_);
        ui->plotWidget->addDrawable(mesh_);
        calculusEditor_->enable();
    }

    tabWidgetIndex_ = index;
    this->update();
}
