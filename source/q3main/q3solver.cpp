#include <QDebug>

#include "q3solver.h"
#include "q3movedirector.h"
#include "q3sceletoneditor.h"
#include "q3boundaryfixedvelocity.h"
#include "q3ani2dmeshadapter.h"
#include "q3testbuilder.h"
#include "ui_q3solver.h"

Q3Solver::Q3Solver(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::Q3Solver),
    sceletonEditor_(NULL),
    meshEditor_(NULL),
    boundaryEditor_(NULL),
    calculusEditor_(NULL),
    sceleton_(this),
    tabWidgetIndex_(-1)
{
    ui->setupUi(this);
    meshEditor_ = new Q3MeshEditor(ui->plotWidget, mesh_, sceleton_,
                                   boundaries_, this);
    calculusEditor_ = new Q3CalculusEditor(ui->plotWidget, mesh_, this);

    Q3FlowPastCylinderTestBuilder testBuilder;
    testBuilder.buildTest(sceleton_, mesh_,
                          boundaries_, meshEditor_->meshAdapter());

    ui->meshEditorLayout->addWidget(meshEditor_);
    ui->calculusEditorLayout->addWidget(calculusEditor_);

    on_tabWidget_currentChanged(0);
}

Q3Solver::~Q3Solver()
{
    // TODO: рвзобраться почему сам не удаляется
    delete meshEditor_;
    delete calculusEditor_;
    delete ui;
}

void Q3Solver::paintEvent(QPaintEvent *event)
{
}

Q3Mesh& Q3Solver::mesh()
{
    return mesh_;
}

Q3Sceleton &Q3Solver::sceleton()
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
        ui->plotWidget->addDrawable(&sceleton_);
        sceletonEditor_ = new Q3SceletonEditor(ui->plotWidget, sceleton_);
        ui->sceletonEditorLayout->addWidget(sceletonEditor_);

        connect(sceletonEditor_, SIGNAL(goToTab(int)),
                ui->tabWidget, SLOT(setCurrentIndex(int)));
    }
    else if (index == 1)
    {
        ui->plotWidget->addDrawable(&sceleton_);
        boundaryEditor_ = new Q3BoundaryEditor(ui->plotWidget,
                                               sceleton_, boundaries_);
        ui->boundaryEditorLayout->addWidget(boundaryEditor_);

        connect(boundaryEditor_, SIGNAL(goToTab(int)),
                ui->tabWidget, SLOT(setCurrentIndex(int)));
    }
    else if (index == 2)
    {
//        ui->plotWidget->addDrawable(sceleton_);
        ui->plotWidget->addDrawable(&mesh_);
        meshEditor_->enable();
//        ui->meshEditorLayout->addWidget(meshEditor_);

        connect(meshEditor_, SIGNAL(goToTab(int)),
                ui->tabWidget, SLOT(setCurrentIndex(int)));
    }
    else if (index == 3)
    {
//        ui->plotWidget->addDrawable(sceleton_);
        ui->plotWidget->addDrawable(&mesh_);
        calculusEditor_->enable();
    }

    tabWidgetIndex_ = index;
    this->update();
}
