#include <QDebug>

#include "q3calculuseditor.h"
#include "q3externalplot.h"
#include "q3movedirector.h"
#include "q3pointinfodirector.h"
#include "q3graphs.h"
#include "q3meshinterpolation.h"
#include "q3contoursettingswidget.h"
#include "q3contourdirector.h"
#include "q3vxbyysettingswidget.h"
#include "q3vybyxsettingswidget.h"
#include "ui_q3calculuseditor.h"

Q3CalculusEditor::Q3CalculusEditor(Q3Plot *plot, Q3Mesh &mesh, QWidget *parent) :
    QWidget(parent),
    mesh_(mesh),
    plot_(plot),
    contourPlot_(NULL),
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
    bool monotoneTerm = ui->monotoneTermCheckBox->isChecked();
    calc_->setMonotoneTerm(monotoneTerm);

    connect(calc_, SIGNAL(updateInfo()), this, SLOT(updateInfo()));
}

Q3CalculusEditor::~Q3CalculusEditor()
{
    delete calc_;
    delete ui;
}

void Q3CalculusEditor::enable()
{
    if (enabled_)
        return;

    mesh_.setDrawPolicy(Q3Mesh::DrawBorders | Q3Mesh::DrawVelocity);

    directorManager_ = new Q3DirectorManager(this);
    directorManager_->addDirector(new Q3MoveDirector(directorManager_));
    directorManager_->addDirector(new Q3PointInfoDirector(mesh_, directorManager_));
    directorManager_->setPlot(plot_);
}

void Q3CalculusEditor::disable()
{
    enabled_ = false;
    delete directorManager_;
    directorManager_ = NULL;
    plot_->removeDrawable(contourPlot_);
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
    calc_->setTau(tau);
    calc_->setRe(Re);

    bool badTriangleFix = ui->badTrianglesFixCheckBox->isChecked();
    calc_->setBadTriangleFix(badTriangleFix);
    bool monotoneTerm = ui->monotoneTermCheckBox->isChecked();
    calc_->setMonotoneTerm(monotoneTerm);

    calc_->start();
}

void Q3CalculusEditor::on_stopCalcButton_clicked()
{
    calc_->abort();
}

void Q3CalculusEditor::on_resetCalcButton_clicked()
{
    calc_->reset();

    for (int i = 0; i < mesh_.triangles().count(); ++i)
    {
        Q3MeshTriangle *triangle = mesh_.triangles().at(i);
        triangle->setCorrectorVelocity(QVector2D(0, 0));
        triangle->setPredictorVelocity(QVector2D(0, 0));
        triangle->setStream(0);
    }

    for (int i = 0; i < mesh_.edges().count(); ++i)
    {
        Q3MeshEdge *edge = mesh_.edges().at(i);
        edge->setVelocity(QVector2D(0, 0));
        edge->setPreassure(0);
    }
}

void Q3CalculusEditor::on_internalClearPlotButton_clicked()
{
    plot_->removeDrawable(contourPlot_);
    delete contourPlot_;
    contourPlot_ = NULL;
    plot_->update();
}

void Q3CalculusEditor::on_internalStreamPlotButton_clicked()
{
    plot_->removeDrawable(contourPlot_);
    delete contourPlot_;
    contourPlot_ = new Q3StreamPlot(mesh_);
    plot_->addDrawable(contourPlot_);
    plot_->update();
}

void Q3CalculusEditor::on_externalStreamPlotButton_clicked()
{
    Q3StreamPlot *streamPlot = new Q3StreamPlot(mesh_);
    Q3ExternalPlot *plot = new Q3ExternalPlot(this);
    connect(plot, SIGNAL(updatePlot()), streamPlot, SLOT(update()));

    plot->addDrawable(streamPlot);
    plot->addSettingsWidget(new Q3ContourSettingsWidget(*streamPlot, plot));
    plot->addDirector(new Q3ContourDirector(mesh_, *streamPlot));
    plot->addDirector(new Q3PointInfoDirector(mesh_));
    plot->plotWidget()->setSceneRect(mesh_.boundingRect());
}

void Q3CalculusEditor::on_internalVorticityPlotButton_clicked()
{
    plot_->removeDrawable(contourPlot_);
    delete contourPlot_;
    contourPlot_ = new Q3VorticityPlot(mesh_);
    plot_->addDrawable(contourPlot_);
    plot_->update();
}

void Q3CalculusEditor::on_externalVorticityPlotButton_clicked()
{
    Q3VorticityPlot *vorticityPlot = new Q3VorticityPlot(mesh_);
    Q3ExternalPlot *plot = new Q3ExternalPlot(this);
    connect(plot, SIGNAL(updatePlot()), vorticityPlot, SLOT(update()));
    plot->addDrawable(vorticityPlot);
    plot->addDirector(new Q3ContourDirector(mesh_, *vorticityPlot));
    plot->addDirector(new Q3PointInfoDirector(mesh_));
    plot->plotWidget()->setSceneRect(mesh_.boundingRect());
}

void Q3CalculusEditor::on_internalPreassurePlotButton_clicked()
{
    plot_->removeDrawable(contourPlot_);
    delete contourPlot_;
    contourPlot_ = new Q3PreassurePlot(mesh_);
    plot_->addDrawable(contourPlot_);
    plot_->update();
}

void Q3CalculusEditor::on_externalPreassurePlotButton_clicked()
{
    Q3PreassurePlot *preassurePlot = new Q3PreassurePlot(mesh_);
    Q3ExternalPlot *plot = new Q3ExternalPlot(this);
    connect(plot, SIGNAL(updatePlot()), preassurePlot, SLOT(update()));
    plot->addDrawable(preassurePlot);
    plot->addDirector(new Q3ContourDirector(mesh_, *preassurePlot));
    plot->addDirector(new Q3PointInfoDirector(mesh_));
    plot->plotWidget()->setSceneRect(mesh_.boundingRect());
}

void Q3CalculusEditor::on_internalMagnitudePlotButton_clicked()
{
    plot_->removeDrawable(contourPlot_);
    delete contourPlot_;
    contourPlot_ = new Q3MagnitudePlot(mesh_);
    plot_->addDrawable(contourPlot_);
    plot_->update();
}

void Q3CalculusEditor::on_externalMagnitudePlotButton_clicked()
{
    Q3MagnitudePlot *magnitudePlot = new Q3MagnitudePlot(mesh_);
    Q3ExternalPlot *plot = new Q3ExternalPlot(this);
    connect(plot, SIGNAL(updatePlot()), magnitudePlot, SLOT(update()));
    plot->addDrawable(magnitudePlot);
    plot->addDirector(new Q3ContourDirector(mesh_, *magnitudePlot));
    plot->addDirector(new Q3PointInfoDirector(mesh_));
    plot->plotWidget()->setSceneRect(mesh_.boundingRect());
}

void Q3CalculusEditor::on_externalVXButton_clicked()
{
    Q3VxByYPlot *vXbyYplot = new Q3VxByYPlot(mesh_);
    Q3ExternalPlot *plot = new Q3ExternalPlot(this);
    plot->plotWidget()->setXLabel("x");
    plot->plotWidget()->setYLabel("u(x0,y)");
    plot->addSettingsWidget(
                new Q3VxByYSettingsWidget(*vXbyYplot,
                                          mesh_.boundingRect().left(),
                                          mesh_.boundingRect().right(),
                                          plot));
    plot->addDrawable(vXbyYplot);
    plot->plotWidget()->setSceneRect(vXbyYplot->boundingRect());

    connect(plot, SIGNAL(updatePlot()), vXbyYplot, SLOT(update()));
}

void Q3CalculusEditor::on_externalVYButton_clicked()
{
    Q3VyByXPlot *vYbyXplot = new Q3VyByXPlot(mesh_);
    Q3ExternalPlot *plot = new Q3ExternalPlot(this);
    plot->plotWidget()->setXLabel("x");
    plot->plotWidget()->setYLabel("v(x,y0)");
    plot->addSettingsWidget(
                new Q3VyByXSettingsWidget(*vYbyXplot,
                                          mesh_.boundingRect().top(),
                                          mesh_.boundingRect().bottom(),
                                          plot));
    plot->addDrawable(vYbyXplot);
    plot->plotWidget()->setSceneRect(vYbyXplot->boundingRect());

    connect(plot, SIGNAL(updatePlot()), vYbyXplot, SLOT(update()));
}
