#include <QDebug>
#include <QFileDialog>

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

    directorManager_ = new Q3DirectorManager(this);
    Q3Director *moveDirector = new Q3MoveDirector(directorManager_);
    directorManager_->addDirector(moveDirector);
    directorManager_->setPlot(ui->plotWidget);
}

Q3ExternalPlot::~Q3ExternalPlot()
{
    delete ui;
}

Q3Plot *Q3ExternalPlot::plotWidget()
{
    return ui->plotWidget;
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
