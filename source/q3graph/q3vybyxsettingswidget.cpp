#include <QDebug>

#include "q3vybyxsettingswidget.h"
#include "ui_q3vybyxsettingswidget.h"

Q3VyByXSettingsWidget::Q3VyByXSettingsWidget(Q3VyByXPlot &vYByXPlot, qreal minY,
                                             qreal maxY, QWidget *parent) :
    Q3PlotSettingsWidget(parent),
    ui(new Ui::Q3VyByXSettingsWidget),
    vYByXPlot_(vYByXPlot),
    minY_(minY),
    maxY_(maxY)
{
    ui->setupUi(this);
    ui->ySpinBox->setMinimum(minY_);
    ui->ySpinBox->setMaximum(maxY_);
    ui->ySpinBox->setValue(0.5 * (minY_ + maxY_));
    updatePlot();
}

Q3VyByXSettingsWidget::~Q3VyByXSettingsWidget()
{
    delete ui;
}

void Q3VyByXSettingsWidget::on_ySpinBox_valueChanged(qreal value)
{
    updatePlot();
}

void Q3VyByXSettingsWidget::updatePlot()
{
    vYByXPlot_.setYValue(ui->ySpinBox->value());
    vYByXPlot_.update();

    if (plot_)
        plot_->update();
}
