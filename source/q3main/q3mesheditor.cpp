#include "q3mesheditor.h"
#include "q3ani2dmeshadapter.h"
#include "q3movedirector.h"
#include "ui_q3mesheditor.h"

Q3MeshEditor::Q3MeshEditor(Q3Plot *plot, Q3Mesh *mesh,
                           Q3Sceleton *sceleton,
                           QList<Q3Boundary *> *boundaries, QWidget *parent) :
    QWidget(parent),
    plot_(plot),
    mesh_(mesh),
    sceleton_(sceleton),
    boundaries_(boundaries),
    meshAdapter_(new Q3Ani2DMeshAdapter),
    ui(new Ui::Q3MeshEditor)
{
    ui->setupUi(this);

    directorManager_ = new Q3DirectorManager(this);
    Q3Director *moveDirector = new Q3MoveDirector(this);
    directorManager_->addDirector(moveDirector);
    directorManager_->setPlot(plot_);
}

Q3MeshEditor::~Q3MeshEditor()
{
    delete ui;
    delete meshAdapter_;
}

void Q3MeshEditor::on_createMeshButton_clicked()
{
    Q3Boundary::setUniqLabels(boundaries_);
    meshAdapter_->generateMesh(sceleton_, boundaries_);
    meshAdapter_->meshToQ3Mesh(mesh_, boundaries_);

    ui->meshInfoLabel->setText(mesh_->info());

    plot_->update();

//    emit goToTab(3);
}

void Q3MeshEditor::on_meshParameter_count_clicked()
{
    meshAdapter_->setSizePolicy(Q3MeshAdapter::ElementSizeByCount);
}

void Q3MeshEditor::on_meshParameter_size_clicked()
{
    meshAdapter_->setSizePolicy(Q3MeshAdapter::ElementSizeBySize);
}

void Q3MeshEditor::on_meshParameter_auto_clicked()
{
    meshAdapter_->setSizePolicy(Q3MeshAdapter::ElementSizeAuto);
}

void Q3MeshEditor::on_elementsCountSlider_valueChanged(int value)
{
    ui->meshParameter_count->setChecked(true);
    meshAdapter_->setSizePolicy(Q3MeshAdapter::ElementSizeByCount);
    meshAdapter_->setElementsCount(value);
    ui->elementsCountSpinBox->setValue(value);
}

void Q3MeshEditor::on_elementsCountSpinBox_valueChanged(int arg1)
{
    ui->meshParameter_count->setChecked(true);
    meshAdapter_->setSizePolicy(Q3MeshAdapter::ElementSizeByCount);
    meshAdapter_->setElementsCount(arg1);
    ui->elementsCountSlider->setValue(arg1);
}

void Q3MeshEditor::on_elementSizeSlider_valueChanged(int value)
{
    ui->meshParameter_size->setChecked(true);
    meshAdapter_->setSizePolicy(Q3MeshAdapter::ElementSizeBySize);
    meshAdapter_->setElementSize(value / 1000.);
    ui->elementSizeSpinBox->setValue(value / 1000.);
}

void Q3MeshEditor::on_elementSizeSpinBox_valueChanged(double arg1)
{
    ui->meshParameter_size->setChecked(true);
    meshAdapter_->setSizePolicy(Q3MeshAdapter::ElementSizeBySize);
    meshAdapter_->setElementSize(arg1);
    ui->elementSizeSlider->setValue(arg1 * 1000.);
}

void Q3MeshEditor::on_removeMeshButton_clicked()
{
    mesh_->clear();
    ui->meshInfoLabel->clear();
    plot_->update();
}

void Q3MeshEditor::on_saveMeshButton_clicked()
{
    meshAdapter_->saveMesh();
}
