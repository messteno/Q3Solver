#include <math.h>

#include <QMessageBox>
#include <QDebug>
#include <QLabel>

#include "q3movedirector.h"
#include "q3additemdirector.h"
#include "q3selectdirector.h"
#include "q3resizedirector.h"
#include "q3pointconnection.h"
#include "q3point.h"
#include "q3circle.h"
#include "q3meshbuilder.h"
#include "ui_q3meshbuilder.h"

Q3MeshBuilder::Q3MeshBuilder(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::Q3MeshBuilder),
    sceleton_(NULL),
    mesh_(NULL),
    meshAdapter_(new Q3Ani2DMeshAdapter)
{
    ui->setupUi(this);

    mesh_ = new Q3Mesh(this);
    sceleton_ = new Q3Sceleton(this);

    Q3Director *addItemDirector = new Q3AddItemDirector(this);
    Q3Director *selectDirector = new Q3SelectDirector(this);
    Q3Director *moveDirector = new Q3MoveDirector(this);
    Q3Director *resizeDirector = new Q3ResizeDirector(this);
    directors_.append(addItemDirector);
    directors_.append(selectDirector);
    directors_.append(resizeDirector);
    directors_.append(moveDirector);

    foreach (Q3Director *director, directors_)
    {
        director->setSceleton(sceleton_);
        director->setPlot(ui->plotWidget);
    }

    ui->directorsLayout->addWidget(addItemDirector);
    ui->directorsLayout->addWidget(selectDirector);

    connect(resizeDirector, SIGNAL(resized()),
            selectDirector, SLOT(setupEditForm()));

    connect(selectDirector, SIGNAL(itemMoved()),
            addItemDirector, SLOT(setupAddForm()));

    ui->plotWidget->addDrawable(static_cast<Q3PlotDrawable *>(mesh_));
    ui->plotWidget->addDrawable(static_cast<Q3PlotDrawable *>(sceleton_));
    ui->plotWidget->addDrawable(static_cast<Q3PlotDrawable *>(addItemDirector));

    connect(ui->plotWidget, SIGNAL(mouseClicked(QMouseEvent *, const QPointF)),
            this, SLOT(plotMouseClicked(QMouseEvent *, const QPointF)));

    connect(ui->plotWidget, SIGNAL(mouseDragged(const QPointF, const QPointF)),
            this, SLOT(plotMouseDragged(const QPointF, const QPointF)));

    connect(ui->plotWidget, SIGNAL(mouseDropped(const QPointF)),
            this, SLOT(plotMouseDropped(const QPointF)));

    connect(ui->plotWidget, SIGNAL(mouseMoved(const QPointF, const QPointF)),
            this, SLOT(plotMouseMoved(const QPointF, const QPointF)));

    connect(sceleton_, SIGNAL(createMeshProgress(int)),
            this->ui->actionProgress, SLOT(setValue(int)));

    ui->meshParametersBox->setEnabled(false);
}

Q3MeshBuilder::~Q3MeshBuilder()
{
    delete ui;
    delete meshAdapter_;
    directors_.clear();
}

void Q3MeshBuilder::keyReleaseEvent(QKeyEvent *event)
{
    QList<Q3Director *> orderedByActivityDirectors =
            Q3Director::orderListByActivity(directors_);
    foreach(Q3Director *director, orderedByActivityDirectors)
    {
        if (!director->isEnabled())
            continue;

        bool processed = director->processKeyRelease(event->key(),
                                                     ui->snapToGrid->isChecked());
        if (processed)
        {
            ui->plotWidget->update();
            break;
        }
    }
}

void Q3MeshBuilder::plotMouseClicked(QMouseEvent *event, const QPointF &scenePos)
{
    QList<Q3Director *> orderedByActivityDirectors =
            Q3Director::orderListByActivity(directors_);
    foreach(Q3Director *director, orderedByActivityDirectors)
    {
        if (!director->isEnabled())
            continue;

        bool processed = director->processClick(event, scenePos,
                                                ui->snapToGrid->isChecked());
        if (processed)
            break;
    }
    ui->plotWidget->update();
}

void Q3MeshBuilder::plotMouseDragged(const QPointF &oldScenePos,
                                     const QPointF &newScenePos)
{
    QList<Q3Director *> orderedByActivityDirectors =
            Q3Director::orderListByActivity(directors_);
    foreach(Q3Director *director, orderedByActivityDirectors)
    {
        if (!director->isEnabled())
            continue;

        bool processed = director->processDragged(oldScenePos,
                                                  newScenePos,
                                                  ui->snapToGrid->isChecked());
        if (processed)
            break;
    }
    ui->plotWidget->update();
}

void Q3MeshBuilder::plotMouseDropped(const QPointF &scenePos)
{
    QList<Q3Director *> orderedByActivityDirectors =
            Q3Director::orderListByActivity(directors_);
    foreach(Q3Director *director, orderedByActivityDirectors)
    {
        if (!director->isEnabled())
            continue;

        bool processed = director->processDropped(scenePos,
                                                  ui->snapToGrid->isChecked());
        if (processed)
            break;
    }
    ui->plotWidget->update();
}

void Q3MeshBuilder::plotMouseMoved(const QPointF &oldScenePos,
                                   const QPointF &newScenePos)
{
    QList<Q3Director *> orderedByActivityDirectors =
            Q3Director::orderListByActivity(directors_);
    foreach(Q3Director *director, orderedByActivityDirectors)
    {
        if (!director->isEnabled())
            continue;

        bool processed = director->processMoved(oldScenePos,
                                                newScenePos,
                                                ui->snapToGrid->isChecked());
        if (processed)
            break;
    }
    ui->plotWidget->update();
}

void Q3MeshBuilder::on_pointButton_clicked(bool checked)
{
    if (checked)
    {
        ui->pointConnectionButton->setChecked(false);
        ui->circleButton->setChecked(false);
        foreach (Q3Director *director, directors_)
            director->setItemType(Q3SceletonItem::Point);
    }
    else
    {
        foreach (Q3Director *director, directors_)
            director->setItemType(Q3SceletonItem::Base);
    }
    ui->plotWidget->update();
}

void Q3MeshBuilder::on_pointConnectionButton_clicked(bool checked)
{
    if (checked)
    {
        ui->pointButton->setChecked(false);
        ui->circleButton->setChecked(false);
        foreach (Q3Director *director, directors_)
            director->setItemType(Q3SceletonItem::PointConnection);
    }
    else
    {
        foreach (Q3Director *director, directors_)
            director->setItemType(Q3SceletonItem::Base);
    }
    ui->plotWidget->update();
}

void Q3MeshBuilder::on_circleButton_clicked(bool checked)
{
    if (checked)
    {
        ui->pointButton->setChecked(false);
        ui->pointConnectionButton->setChecked(false);
        foreach (Q3Director *director, directors_)
            director->setItemType(Q3SceletonItem::Circle);
    }
    else
    {
        foreach (Q3Director *director, directors_)
            director->setItemType(Q3SceletonItem::Base);
    }
    ui->plotWidget->update();
}

void Q3MeshBuilder::on_createMeshButton_clicked()
{
    ui->actionProgress->setValue(0);

    foreach (Q3Director *director, directors_)
        director->stop();

    ui->actionProgress->setValue(5);

    if (ui->autoParameters->isChecked())
        meshAdapter_->setSizePolicy(Q3MeshAdapter::ElementSizeAuto);
    else if (ui->meshParameter_1->isChecked())
    {
        meshAdapter_->setSizePolicy(Q3MeshAdapter::ElementSizeByCount);
        meshAdapter_->setElementsCount(ui->elementsCountSpinBox->value());
    }
    else
    {
        meshAdapter_->setSizePolicy(Q3MeshAdapter::ElementSizeBySize);
        meshAdapter_->setElementSize(ui->elementSizeSpinBox->value());
    }

    if (sceleton_->createMesh(meshAdapter_) &&
        meshAdapter_->meshToQ3Mesh(mesh_))
    {
        foreach (Q3Director *director, directors_)
        {
            if (director->type() != Q3Director::Move)
                director->setEnabled(false);
        }

        ui->pointButton->setEnabled(false);
        ui->pointConnectionButton->setEnabled(false);
        ui->circleButton->setEnabled(false);
        ui->createMeshButton->setEnabled(false);
        ui->removeMeshButton->setEnabled(true);

        ui->actionProgress->setValue(100);
    }
    else
    {
        ui->actionProgress->setValue(0);
    }

    update();
}

void Q3MeshBuilder::on_removeMeshButton_clicked()
{
    mesh_->clear();
    ui->pointButton->setEnabled(true);
    ui->pointConnectionButton->setEnabled(true);
    ui->circleButton->setEnabled(true);
    ui->createMeshButton->setEnabled(true);
    ui->removeMeshButton->setEnabled(false);
    foreach (Q3Director *director, directors_)
        director->setEnabled(true);
    ui->actionProgress->setValue(0);
    update();
}

void Q3MeshBuilder::on_autoParameters_toggled(bool checked)
{
    ui->meshParametersBox->setEnabled(!checked);
}

void Q3MeshBuilder::on_elementsCountSlider_valueChanged(int value)
{
    ui->elementsCountSpinBox->setValue(value);
}

void Q3MeshBuilder::on_elementsCountSpinBox_valueChanged(int arg1)
{
    ui->elementsCountSlider->setValue(arg1);
}

void Q3MeshBuilder::on_elementSizeSlider_valueChanged(int value)
{
    ui->elementSizeSpinBox->setValue(0.01 * value);
}

void Q3MeshBuilder::on_elemetSizeSpinBox_valueChanged(double arg1)
{
    ui->elementSizeSlider->setValue(100 * arg1);
}
