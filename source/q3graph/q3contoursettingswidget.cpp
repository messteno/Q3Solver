#include <QStringListModel>
#include <QtAlgorithms>

#include "q3contoursettingswidget.h"
#include "ui_q3contoursettingswidget.h"

Q3ContourSettingsWidget::Q3ContourSettingsWidget(Q3ContourPlot &contourPlot,
                                                 QWidget *parent) :
    Q3PlotSettingsWidget(parent),
    ui(new Ui::Q3ContourSettingsWidget),
    contourPlot_(contourPlot)
{
    ui->setupUi(this);
    ui->coloursCountSpinBox->setValue(255);
}

Q3ContourSettingsWidget::~Q3ContourSettingsWidget()
{
    delete ui;
}

void Q3ContourSettingsWidget::on_coloursCountSpinBox_valueChanged(int value)
{
    contourPlot_.setLevels(value, true);
    contourPlot_.createFilledContour();
    if (plot_)
        plot_->update();
}

void Q3ContourSettingsWidget::on_isolinesCheckBox_toggled(bool checked)
{
    contourPlot_.setShowLines(checked);
    if (plot_)
        plot_->update();
}
