#include <QDebug>
#include <QFileDialog>
#include <QApplication>
#include <QDesktopWidget>

#include "q3externalplot.h"
#include "q3movedirector.h"
#include "ui_q3externalplot.h"

Q3ExternalPlot::Q3ExternalPlot(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::Q3ExternalPlot)
{
    ui->setupUi(this);
    ui->plotWidget->setBackgroundColor(Qt::white);
    ui->plotWidget->setForegroundColor(Qt::black);
    ui->plotWidget->setAxesColor(Qt::gray);

    connect(this, SIGNAL(updatePlot()), ui->plotWidget, SLOT(update()));

    directorManager_ = new Q3DirectorManager(this);
    Q3Director *moveDirector = new Q3MoveDirector(directorManager_);
    directorManager_->addDirector(moveDirector);
    directorManager_->setPlot(ui->plotWidget);

    move(QApplication::desktop()->rect().center() - rect().center());
    setAttribute(Qt::WA_DeleteOnClose, true);
    show();
}

Q3ExternalPlot::~Q3ExternalPlot()
{
    delete ui;
    foreach (Q3PlotDrawable *drawable, drawables_)
        delete drawable;
    drawables_.clear();
}

Q3Plot *Q3ExternalPlot::plotWidget()
{
    return ui->plotWidget;
}

void Q3ExternalPlot::addDrawable(Q3PlotDrawable *drawable)
{
    drawables_.append(drawable);
    ui->plotWidget->addDrawable(drawable);
}

void Q3ExternalPlot::addDirector(Q3Director *director)
{
    directorManager_->addDirector(director);
    director->setParent(directorManager_);
    director->setPlot(ui->plotWidget);
}

void Q3ExternalPlot::addSettingsWidget(Q3PlotSettingsWidget *widget)
{
    ui->settingsLayout->addWidget(widget);
    widget->setPlot(ui->plotWidget);
}

void Q3ExternalPlot::on_closeButton_clicked()
{
    this->close();
}

void Q3ExternalPlot::on_savePlotButton_clicked()
{
    QFileDialog *fileDialog = new QFileDialog(this);
    QString path = fileDialog->getSaveFileName(this, tr("Save as image"),
                                               "../data",
                                               tr("PNG file (*.png)"));

    if (path.isEmpty())
        return;

    QFileInfo file(path);
    if (file.suffix().isEmpty())
        path += ".png";

    QImage img(ui->plotWidget->size(), QImage::Format_ARGB32);

    QPainter painter(&img);
    ui->plotWidget->render(&painter);

    img.save(path);
}

void Q3ExternalPlot::on_updateButton_clicked()
{
    emit updatePlot();
}
