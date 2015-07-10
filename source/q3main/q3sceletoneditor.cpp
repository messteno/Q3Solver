#include <math.h>

#include <QMessageBox>
#include <QDebug>
#include <QLabel>
#include <QFileDialog>

#include "q3movedirector.h"
#include "q3additemdirector.h"
#include "q3selectdirector.h"
#include "q3resizedirector.h"
#include "q3pointconnection.h"
#include "q3point.h"
#include "q3circle.h"
#include "q3sceletoneditor.h"
#include "ui_q3sceletoneditor.h"

Q3SceletonEditor::Q3SceletonEditor(Q3Plot *plot, Q3Sceleton &sceleton,
                                   QWidget *parent) :
    QWidget(parent),
    ui(new Ui::Q3MeshBuilder),
    sceleton_(sceleton),
    plot_(plot)
{
    ui->setupUi(this);

    directorManager_ = new Q3DirectorManager(this);

    Q3Director *addItemDirector = new Q3AddItemDirector(this);
    Q3SelectDirector *selectDirector = new Q3SelectDirector(this);
    selectDirector->setMovable(true);
    selectDirector->setEditable(true);
    Q3Director *moveDirector = new Q3MoveDirector(this);
    Q3Director *resizeDirector = new Q3ResizeDirector(this);

    directorManager_->addDirector(addItemDirector);
    directorManager_->addDirector(selectDirector);
    directorManager_->addDirector(resizeDirector);
    directorManager_->addDirector(moveDirector);

    foreach (Q3Director *director, directorManager_->directors())
        director->setSceleton(&sceleton_);

    directorManager_->setPlot(plot_);

    ui->directorsLayout->addWidget(addItemDirector);
    ui->directorsLayout->addWidget(selectDirector);

    connect(resizeDirector, SIGNAL(itemResized()),
            selectDirector, SLOT(setupEditForm()));
    connect(resizeDirector, SIGNAL(itemResized()),
            &sceleton_, SLOT(itemsUpdated()));

    connect(selectDirector, SIGNAL(itemMoved()),
            addItemDirector, SLOT(setupAddForm()));
    connect(selectDirector, SIGNAL(itemMoved()),
            &sceleton_, SLOT(itemsUpdated()));

    plot_->addDrawable(static_cast<Q3PlotDrawable *>(addItemDirector));

//    connect(sceleton_, SIGNAL(createMeshProgress(int)),
//            this->ui->actionProgress, SLOT(setValue(int)));

//    ui->meshParametersBox->setEnabled(false);

    ui->elementsTableView->setModel(&sceleton_);
    ui->elementsTableView->horizontalHeader()->setSectionResizeMode(
                QHeaderView::ResizeToContents);

    showFullScreen();
}

Q3SceletonEditor::~Q3SceletonEditor()
{
    delete ui;
}

void Q3SceletonEditor::on_pointButton_clicked(bool checked)
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
    plot_->update();
}

void Q3SceletonEditor::on_pointConnectionButton_clicked(bool checked)
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
    plot_->update();
}

void Q3SceletonEditor::on_circleButton_clicked(bool checked)
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
    plot_->update();
}

void Q3SceletonEditor::on_snapToGrid_toggled(bool checked)
{
    plot_->setSnapToGrid(checked);
}

void Q3SceletonEditor::on_prepareSceletonButton_clicked()
{
    bool ok = sceleton_.prepare();
    if (ok)
    {
        emit goToTab(1);
        return;
    }
}

void Q3SceletonEditor::on_saveSceletonButton_clicked()
{
    QFileDialog *fileDialog = new QFileDialog(this);
    QString path = fileDialog->getSaveFileName(this, tr("Save sceleton"),
                                               "../data",
                                               tr("Q3S file (*.q3s)"));

    if (path.isEmpty())
        return;

    QFileInfo file(path);
    if (file.suffix() != "q3s")
        path += ".q3s";

    sceleton_.save(path);
}
