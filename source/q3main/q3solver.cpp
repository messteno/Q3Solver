#include "q3solver.h"
#include "q3movedirector.h"
#include "q3sceletoneditor.h"
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

    mesh_ = new Q3Mesh(this);
    ui->plotWidget->addDrawable(static_cast<Q3PlotDrawable *>(mesh_));

    sceleton_ = new Q3Sceleton(this);

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
        delete meshEditor_;
        meshEditor_ = NULL;
    }
    else if (tabWidgetIndex_ == 3)
    {
        delete calculusEditor_;
        calculusEditor_ = NULL;
    }

    if (index == 0)
    {
        ui->plotWidget->addDrawable(static_cast<Q3PlotDrawable *>(sceleton_));
        sceletonEditor_ = new Q3SceletonEditor(ui->plotWidget, sceleton_);
        ui->sceletonEditorLayout->addWidget(sceletonEditor_);

        connect(sceletonEditor_, SIGNAL(goToTab(int)),
                ui->tabWidget, SLOT(setCurrentIndex(int)));
    }
    else if (index == 1)
    {
        ui->plotWidget->addDrawable(static_cast<Q3PlotDrawable *>(sceleton_));
        boundaryEditor_ = new Q3BoundaryEditor(ui->plotWidget,
                                               sceleton_, &boundaries_);
        ui->boundaryEditorLayout->addWidget(boundaryEditor_);

        connect(boundaryEditor_, SIGNAL(goToTab(int)),
                ui->tabWidget, SLOT(setCurrentIndex(int)));
    }
    else if (index == 2)
    {
        ui->plotWidget->addDrawable(static_cast<Q3PlotDrawable *>(sceleton_));
        ui->plotWidget->addDrawable(static_cast<Q3PlotDrawable *>(mesh_));
        meshEditor_ = new Q3MeshEditor(ui->plotWidget, mesh_, sceleton_,
                                       &boundaries_);
        ui->meshEditorLayout->addWidget(meshEditor_);

        connect(meshEditor_, SIGNAL(goToTab(int)),
                ui->tabWidget, SLOT(setCurrentIndex(int)));
    }
    else if (index == 3)
    {
//        ui->plotWidget->addDrawable(mesh_);
        ui->plotWidget->addDrawable(sceleton_);
        calculusEditor_ = new Q3CalculusEditor(ui->plotWidget, mesh_);
        ui->calculusEditorLayout->addWidget(calculusEditor_);
    }

    tabWidgetIndex_ = index;
    this->update();
}
