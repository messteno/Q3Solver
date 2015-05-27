#include "q3calculuseditor.h"
#include "q3externalplot.h"
#include "q3movedirector.h"
#include "q3meshinterpolation.h"
#include "ui_q3calculuseditor.h"

Q3CalculusEditor::Q3CalculusEditor(Q3Plot *plot, Q3Mesh *mesh, QWidget *parent) :
    QWidget(parent),
    mesh_(mesh),
    plot_(plot),
    contourPlot_(mesh),
    enabled_(false),
    ui(new Ui::Q3CalculusEditor)
{
    ui->setupUi(this);

    qreal meanVelocity = ui->meanVelocityEdit->text().toDouble();
    qreal characteristicLength = ui->characteristicLengthEdit->text().toDouble();
    qreal kinematicViscosity = ui->kinematicViscosityEdit->text().toDouble();
    qreal tau = ui->tauEdit->text().toDouble();
    qreal Re = meanVelocity * characteristicLength / kinematicViscosity;
    calc_ = new Q3Calc(mesh_, tau, Re, this);
    bool badTriangleFix = ui->badTrianglesFixCheckBox->isChecked();
    calc_->setBadTriangleFix(badTriangleFix);

    // TODO:

    connect(calc_, SIGNAL(updateInfo()), this, SLOT(updateInfo()));
}

Q3CalculusEditor::~Q3CalculusEditor()
{
    delete ui;
}

void Q3CalculusEditor::enable()
{
    if (enabled_)
        return;

    mesh_->setDrawPolicy(Q3Mesh::DrawBorders | Q3Mesh::DrawVelocity);

    directorManager_ = new Q3DirectorManager(this);
    Q3Director *moveDirector = new Q3MoveDirector(directorManager_);
    directorManager_->addDirector(moveDirector);
    directorManager_->setPlot(plot_);
}

void Q3CalculusEditor::disable()
{
    enabled_ = false;
    delete directorManager_;
    directorManager_ = NULL;
    plot_->removeDrawable(&contourPlot_);
}

void Q3CalculusEditor::updateInfo()
{
    ui->calcInfoLabel->setText(calc_->info());
}

void Q3CalculusEditor::on_startCalcButton_clicked()
{
    qreal meanVelocity = ui->meanVelocityEdit->text().toDouble();
    qreal characteristicLength = ui->characteristicLengthEdit->text().toDouble();
    qreal kinematicViscosity = ui->kinematicViscosityEdit->text().toDouble();
    qreal tau = ui->tauEdit->text().toDouble();
    qreal Re = meanVelocity * characteristicLength / kinematicViscosity;
    bool badTriangleFix = ui->badTrianglesFixCheckBox->isChecked();
    calc_->setTau(tau);
    calc_->setRe(Re);
    calc_->setBadTriangleFix(badTriangleFix);

    calc_->start();
}

void Q3CalculusEditor::on_stopCalcButton_clicked()
{
    calc_->abort();
}

void Q3CalculusEditor::on_resetCalcButton_clicked()
{
    calc_->reset();

    for (int i = 0; i < mesh_->triangles().count(); ++i)
    {
        Q3MeshTriangle *triangle = mesh_->triangles().at(i);
        triangle->setCorrectorVelocity(QVector2D(0, 0));
        triangle->setPredictorVelocity(QVector2D(0, 0));
    }

    for (int i = 0; i < mesh_->edges().count(); ++i)
    {
        Q3MeshEdge *edge = mesh_->edges().at(i);
        edge->setVelocity(QVector2D(0, 0));
        edge->setPreassure(0);
    }
}

void Q3CalculusEditor::on_internalClearPlotButton_clicked()
{
    plot_->removeDrawable(&contourPlot_);
    plot_->update();
}

void Q3CalculusEditor::on_internalStreamPlotButton_clicked()
{
    mesh_->calcStream();

    QVector<QVector3D> triValues;
    for (int i = 0; i < mesh_->triangles().count(); ++i)
    {
        Q3MeshTriangle *triangle = mesh_->triangles().at(i);
        triValues.append(QVector3D(triangle->center().x(),
                                   triangle->center().y(),
                                   triangle->stream()));
    }
    Q3MeshTriNodeInterpolation interpolation(*mesh_, triValues);
    QVector<qreal> nodeValues = interpolation.interpolateToNodes();

    contourPlot_.clear();
    contourPlot_.setValues(nodeValues);
    contourPlot_.createFilledContour(250);
    contourPlot_.createContour(30);

    plot_->addDrawable(&contourPlot_);
    plot_->update();
}

void Q3CalculusEditor::on_externalStreamPlotButton_clicked()
{
    mesh_->calcStream();

    QVector<QVector3D> triValues;
    for (int i = 0; i < mesh_->triangles().count(); ++i)
    {
        Q3MeshTriangle *triangle = mesh_->triangles().at(i);
        triValues.append(QVector3D(triangle->center().x(),
                                   triangle->center().y(),
                                   triangle->stream()));
    }
    Q3MeshTriNodeInterpolation interpolation(*mesh_, triValues);
    QVector<qreal> nodeValues = interpolation.interpolateToNodes();

    contourPlot_.clear();
    contourPlot_.setValues(nodeValues);
    contourPlot_.createFilledContour(250);
    contourPlot_.createContour(30);

    Q3ExternalPlot *plot = new Q3ExternalPlot(this);
    plot->plotWidget()->setSceneRect(mesh_->boundingRect());
    plot->plotWidget()->addDrawable(&contourPlot_);
}

void Q3CalculusEditor::on_internalVorticityPlotButton_clicked()
{
    mesh_->calcVorticity();

    QVector<QVector3D> triValues;
    for (int i = 0; i < mesh_->triangles().count(); ++i)
    {
        Q3MeshTriangle *triangle = mesh_->triangles().at(i);
        triValues.append(QVector3D(triangle->center().x(),
                                   triangle->center().y(),
                                   triangle->omega()));
    }
    Q3MeshTriNodeInterpolation interpolation(*mesh_, triValues);
    QVector<qreal> nodeValues = interpolation.interpolateToNodes();

    contourPlot_.clear();
    contourPlot_.setValues(nodeValues);
    contourPlot_.createFilledContour(250);

    plot_->addDrawable(&contourPlot_);
    plot_->update();
}

void Q3CalculusEditor::on_externalVorticityPlotButton_clicked()
{
    mesh_->calcVorticity();

    QVector<QVector3D> triValues;
    for (int i = 0; i < mesh_->triangles().count(); ++i)
    {
        Q3MeshTriangle *triangle = mesh_->triangles().at(i);
        triValues.append(QVector3D(triangle->center().x(),
                                   triangle->center().y(),
                                   triangle->omega()));
    }
    Q3MeshTriNodeInterpolation interpolation(*mesh_, triValues);
    QVector<qreal> nodeValues = interpolation.interpolateToNodes();

    contourPlot_.clear();
    contourPlot_.setValues(nodeValues);
    contourPlot_.createFilledContour(250);

    Q3ExternalPlot *plot = new Q3ExternalPlot(this);
    plot->plotWidget()->setSceneRect(mesh_->boundingRect());
    plot->plotWidget()->addDrawable(&contourPlot_);
}

void Q3CalculusEditor::on_internalPreassurePlotButton_clicked()
{
    QVector<QVector3D> edgeValues;
    for (int i = 0; i < mesh_->edges().count(); ++i)
    {
        Q3MeshEdge *edge = mesh_->edges().at(i);
        edgeValues.append(QVector3D(edge->center().x(),
                                    edge->center().y(),
                                    edge->preassure()));
    }
    Q3MeshEdgeNodeInterpolation interpolation(*mesh_, edgeValues);
    QVector<qreal> nodeValues = interpolation.interpolateToNodes();

    contourPlot_.clear();
    contourPlot_.setValues(nodeValues);
    contourPlot_.createFilledContour(250);

    plot_->addDrawable(&contourPlot_);
    plot_->update();
}

void Q3CalculusEditor::on_externalPreassurePlotButton_clicked()
{
    QVector<QVector3D> edgeValues;
    for (int i = 0; i < mesh_->edges().count(); ++i)
    {
        Q3MeshEdge *edge = mesh_->edges().at(i);
        edgeValues.append(QVector3D(edge->center().x(),
                                    edge->center().y(),
                                    edge->preassure()));
    }
    Q3MeshEdgeNodeInterpolation interpolation(*mesh_, edgeValues);
    QVector<qreal> nodeValues = interpolation.interpolateToNodes();

    contourPlot_.clear();
    contourPlot_.setValues(nodeValues);
    contourPlot_.createFilledContour(250);

    Q3ExternalPlot *plot = new Q3ExternalPlot(this);
    plot->plotWidget()->setSceneRect(mesh_->boundingRect());
    plot->plotWidget()->addDrawable(&contourPlot_);
}

void Q3CalculusEditor::on_internalMagnitudePlotButton_clicked()
{
    QVector<QVector3D> triValues;
    for (int i = 0; i < mesh_->triangles().count(); ++i)
    {
        Q3MeshTriangle *triangle = mesh_->triangles().at(i);
        triValues.append(QVector3D(triangle->center().x(),
                                   triangle->center().y(),
                                   triangle->correctorVelocity().length()));
    }
    Q3MeshTriNodeInterpolation interpolation(*mesh_, triValues);
    QVector<qreal> nodeValues = interpolation.interpolateToNodes();

    contourPlot_.clear();
    contourPlot_.setValues(nodeValues);
    contourPlot_.createFilledContour(250);

    plot_->addDrawable(&contourPlot_);
    plot_->update();
}

void Q3CalculusEditor::on_externalMagnitudePlotButton_clicked()
{
    QVector<QVector3D> triValues;
    for (int i = 0; i < mesh_->triangles().count(); ++i)
    {
        Q3MeshTriangle *triangle = mesh_->triangles().at(i);
        triValues.append(QVector3D(triangle->center().x(),
                                   triangle->center().y(),
                                   triangle->correctorVelocity().length()));
    }
    Q3MeshTriNodeInterpolation interpolation(*mesh_, triValues);
    QVector<qreal> nodeValues = interpolation.interpolateToNodes();

    contourPlot_.clear();
    contourPlot_.setValues(nodeValues);
    contourPlot_.createFilledContour(250);

    Q3ExternalPlot *plot = new Q3ExternalPlot(this);
    plot->plotWidget()->setSceneRect(mesh_->boundingRect());
    plot->plotWidget()->addDrawable(&contourPlot_);
}
