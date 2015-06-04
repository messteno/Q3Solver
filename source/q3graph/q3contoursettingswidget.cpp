#include <QStringListModel>
#include <QtAlgorithms>

#include "q3contoursettingswidget.h"
#include "ui_q3contoursettingswidget.h"

Q3ContourSettingsWidget::Q3ContourSettingsWidget(Q3ContourPlot &contourPlot, QWidget *parent) :
    Q3PlotSettingsWidget(parent),
    ui(new Ui::Q3ContourSettingsWidget),
    contourPlot_(contourPlot),
    filled_(true),
    lines_(true)
{
    ui->setupUi(this);
    ui->isolinesCheckbox->setChecked(lines_);
    ui->verticalLayout->setMargin(0);

    QStringList levels_;
    for (int i = 0; i < DefaultContourLevelsCount_; ++i)
        levels_ << QString::number(i / (DefaultContourLevelsCount_ - 1.));

    levelsModel_ = new QStringListModel(this);
    levelsModel_->setStringList(levels_);

    ui->listView->setModel(levelsModel_);
    ui->listView->setSelectionMode(QAbstractItemView::ExtendedSelection);

    updateContour();
}

Q3ContourSettingsWidget::~Q3ContourSettingsWidget()
{
    delete ui;
}

void Q3ContourSettingsWidget::keyPressEvent(QKeyEvent *event)
{
    switch (event->key())
    {
        case Qt::Key_Delete:
        {
            QModelIndexList indexes =
                    ui->listView->selectionModel()->selectedIndexes();
            while (indexes.size())
            {
                levelsModel_->removeRow(indexes.first().row());
                indexes = ui->listView->selectionModel()->selectedIndexes();
            }
            updateContour();
            break;
        }
        default:
            break;
    }
}

void Q3ContourSettingsWidget::on_isolinesCheckbox_clicked(bool checked)
{
    lines_ = checked;
    updateContour();
}

void Q3ContourSettingsWidget::updateContour()
{
    contourPlot_.clear();

    if (filled_)
        contourPlot_.createFilledContour();

    if (lines_)
    {
        QStringList levelsStringList = levelsModel_->stringList();
        QList<qreal> levelsValues;
        foreach(const QString &strLevel, levelsStringList)
            levelsValues << strLevel.toDouble();
        contourPlot_.setContourLevelsList(levelsValues);
        contourPlot_.createContour();
    }
    else
        contourPlot_.setContourLevels(0);

    if (plot_)
        plot_->update();
}

void Q3ContourSettingsWidget::on_addLevelsButton_clicked()
{

    qreal startLevel = ui->startLevelEdit->text().toDouble();
    qreal endLevel = ui->endLevelEdit->text().toDouble();
    qreal stepLevel = ui->stepLevelEdit->text().toDouble();

    qreal level = startLevel;
    QStringList levelsStringList = levelsModel_->stringList();
    while (level < endLevel)
    {
        QString stringLevel = QString::number(level);
        if (!levelsStringList.contains(stringLevel))
            levelsStringList.append(stringLevel);
        level += stepLevel;
    }

    qSort(levelsStringList.begin(), levelsStringList.end());
    levelsModel_->setStringList(levelsStringList);
    updateContour();
}
