#include <math.h>

#include <QMessageBox>
#include <QDebug>
#include <QLabel>

#include "q3ani2dmeshadapter.h"
#include "q3movedirector.h"
#include "q3additemdirector.h"
#include "q3selectdirector.h"
#include "q3resizedirector.h"
#include "q3pointconnection.h"
#include "q3point.h"
#include "q3circle.h"
#include "q3meshbuilder.h"
#include "q3sceletonitemdelegate.h"
#include "ui_q3meshbuilder.h"

Q3MeshBuilder::Q3MeshBuilder(Q3Mesh *mesh, Q3Sceleton *sceleton,
                             QWidget *parent) :
    QDialog(parent),
    ui(new Ui::Q3MeshBuilder),
    mesh_(mesh),
    sceleton_(sceleton),
    meshAdapter_(new Q3Ani2DMeshAdapter)
{
    ui->setupUi(this);

    directorManager_ = new Q3DirectorManager(this);

    Q3Director *addItemDirector = new Q3AddItemDirector(this);
    Q3Director *selectDirector = new Q3SelectDirector(this);
    Q3Director *moveDirector = new Q3MoveDirector(this);
    Q3Director *resizeDirector = new Q3ResizeDirector(this);

    directorManager_->addDirector(addItemDirector);
    directorManager_->addDirector(selectDirector);
    directorManager_->addDirector(resizeDirector);
    directorManager_->addDirector(moveDirector);

    foreach (Q3Director *director, directorManager_->directors())
        director->setSceleton(sceleton_);

    directorManager_->setPlot(ui->plotWidget);

    ui->directorsLayout->addWidget(addItemDirector);
    ui->directorsLayout->addWidget(selectDirector);

    connect(resizeDirector, SIGNAL(itemResized()),
            selectDirector, SLOT(setupEditForm()));
    connect(resizeDirector, SIGNAL(itemResized()),
            sceleton_, SLOT(itemsUpdated()));

    connect(selectDirector, SIGNAL(itemMoved()),
            addItemDirector, SLOT(setupAddForm()));
    connect(selectDirector, SIGNAL(itemMoved()),
            sceleton_, SLOT(itemsUpdated()));

    ui->plotWidget->addDrawable(static_cast<Q3PlotDrawable *>(mesh_));
    ui->plotWidget->addDrawable(static_cast<Q3PlotDrawable *>(sceleton_));
    ui->plotWidget->addDrawable(static_cast<Q3PlotDrawable *>(addItemDirector));

    connect(sceleton_, SIGNAL(createMeshProgress(int)),
            this->ui->actionProgress, SLOT(setValue(int)));

    ui->meshParametersBox->setEnabled(false);

    ui->elementsTableView->setModel(sceleton_);
    ui->elementsTableView->setItemDelegate(new Q3SceletonItemDelegate(this));
    ui->elementsTableView->horizontalHeader()->setSectionResizeMode(
                QHeaderView::ResizeToContents);

    if (!sceleton_->isActive())
    {
        foreach (Q3Director *director, directorManager_->directors())
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

    showFullScreen();
}

Q3MeshBuilder::~Q3MeshBuilder()
{
    delete ui;
    delete meshAdapter_;
}

void Q3MeshBuilder::on_pointButton_clicked(bool checked)
{
    if (checked)
    {
        ui->pointConnectionButton->setChecked(false);
        ui->circleButton->setChecked(false);
        foreach (Q3Director *director, directorManager_->directors())
            director->setItemType(Q3SceletonItem::Point);
    }
    else
    {
        foreach (Q3Director *director, directorManager_->directors())
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
        foreach (Q3Director *director, directorManager_->directors())
            director->setItemType(Q3SceletonItem::PointConnection);
    }
    else
    {
        foreach (Q3Director *director, directorManager_->directors())
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
        foreach (Q3Director *director, directorManager_->directors())
            director->setItemType(Q3SceletonItem::Circle);
    }
    else
    {
        foreach (Q3Director *director, directorManager_->directors())
            director->setItemType(Q3SceletonItem::Base);
    }
    ui->plotWidget->update();
}

void Q3MeshBuilder::on_createMeshButton_clicked()
{
    ui->actionProgress->setValue(0);

    foreach (Q3Director *director, directorManager_->directors())
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

    bool ok = true;

    if (ok)
        ok = sceleton_->prepare();

    if (ok)
        ok = meshAdapter_->generateMesh(sceleton_);

    if (ok)
        ok = meshAdapter_->meshToQ3Mesh(mesh_);

    if (ok)
    {
        foreach (Q3Director *director, directorManager_->directors())
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

        sceleton_->setActive(false);
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
    foreach (Q3Director *director, directorManager_->directors())
        director->setEnabled(true);
    ui->actionProgress->setValue(0);
    sceleton_->setActive(true);

    update();
}

void Q3MeshBuilder::on_autoParameters_toggled(bool checked)
{
    ui->meshParametersBox->setEnabled(!checked);
}

void Q3MeshBuilder::on_elementsCountSlider_valueChanged(int value)
{
    ui->meshParameter_1->setChecked(true);
    ui->elementsCountSpinBox->setValue(value);
}

void Q3MeshBuilder::on_elementsCountSpinBox_valueChanged(int arg1)
{
    ui->elementsCountSlider->setValue(arg1);
}

void Q3MeshBuilder::on_elementSizeSlider_valueChanged(int value)
{
    ui->meshParameter_2->setChecked(true);
    ui->elementSizeSpinBox->setValue(0.01 * value);
}

void Q3MeshBuilder::on_elementSizeSpinBox_valueChanged(double arg1)
{
    ui->elementSizeSlider->setValue(100 * arg1);
}

void Q3MeshBuilder::on_snapToGrid_toggled(bool checked)
{
    ui->plotWidget->setSnapToGrid(checked);
}
