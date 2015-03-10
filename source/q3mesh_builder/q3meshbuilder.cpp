#include <math.h>

#include <QMessageBox>
#include <QDebug>

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
    sceleton_(NULL)
{
    ui->setupUi(this);
    sceleton_ = new Q3Sceleton(this);

    Q3Director *addItemDirector = new Q3AddItemDirector(this);
    Q3Director *selectDirector = new Q3SelectDirector(this);
    Q3Director *moveDirector = new Q3MoveDirector(this);
    Q3Director *resizeDirector = new Q3ResizeDirector(this);
    directors_.append(addItemDirector);
    directors_.append(selectDirector);
    directors_.append(resizeDirector);
    directors_.append(moveDirector);

    ui->directorLayout->addWidget(addItemDirector);
    addItemDirector->show();

    ui->plotWidget->addDrawable(static_cast<Q3PlotDrawable *>(sceleton_));
    ui->plotWidget->addDrawable(static_cast<Q3PlotDrawable *>(addItemDirector));

    connect(ui->plotWidget, SIGNAL(mouseClicked(const QPointF)),
            this, SLOT(plotMouseClicked(const QPointF)));

    connect(ui->plotWidget, SIGNAL(mouseDragged(const QPointF, const QPointF)),
            this, SLOT(plotMouseDragged(const QPointF, const QPointF)));

    connect(ui->plotWidget, SIGNAL(mouseDropped(const QPointF)),
            this, SLOT(plotMouseDropped(const QPointF)));

    connect(ui->plotWidget, SIGNAL(mouseMoved(const QPointF, const QPointF)),
            this, SLOT(plotMouseMoved(const QPointF, const QPointF)));
}

Q3MeshBuilder::~Q3MeshBuilder()
{
    delete ui;
    directors_.clear();
}

void Q3MeshBuilder::keyReleaseEvent(QKeyEvent *event)
{
    QList<Q3Director *> orderedByActivityDirectors =
            Q3Director::orderListByActivity(directors_);
    foreach(Q3Director *director, orderedByActivityDirectors)
    {
        bool processed = director->processKeyRelease(ui->plotWidget,
                                                     sceleton_,
                                                     event->key(),
                                                     ui->snapToGrid->isChecked());
        if (processed)
        {
            ui->plotWidget->update();
            break;
        }
    }
}

void Q3MeshBuilder::plotMouseClicked(const QPointF &scenePos)
{
    QList<Q3Director *> orderedByActivityDirectors =
            Q3Director::orderListByActivity(directors_);
    foreach(Q3Director *director, orderedByActivityDirectors)
    {
        bool processed = director->processClick(ui->plotWidget,
                                                sceleton_,
                                                scenePos,
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
        bool processed = director->processDragged(ui->plotWidget,
                                                  sceleton_,
                                                  oldScenePos,
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
        bool processed = director->processDropped(ui->plotWidget,
                                                  sceleton_,
                                                  scenePos,
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
        bool processed = director->processMoved(ui->plotWidget,
                                                sceleton_,
                                                oldScenePos,
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
    // 1. Disable all directors
    // 2. Try create mesh
    // 3. If creation fails then enable all directors
    if (sceleton_->createMesh() == false)
    {
    }

    update();
}
