#include "q3vxbyysettingswidget.h"
#include "ui_q3vxbyysettingswidget.h"

Q3VxByYSettingsWidget::Q3VxByYSettingsWidget(Q3VxByYPlot &vXByYPlot, qreal minX,
                                             qreal maxX, QWidget *parent) :
    Q3PlotSettingsWidget(parent),
    ui(new Ui::Q3VxByYSettingsWidget),
    vXByYPlot_(vXByYPlot),
    minX_(minX),
    maxX_(maxX)
{
    ui->setupUi(this);
    ui->xSpinBox->setMinimum(minX_);
    ui->xSpinBox->setMaximum(maxX_);
    ui->xSpinBox->setValue(0.5 * (minX_ + maxX_));
    updatePlot();
}

Q3VxByYSettingsWidget::~Q3VxByYSettingsWidget()
{
    delete ui;
}

void Q3VxByYSettingsWidget::on_xSpinBox_valueChanged(qreal value)
{
    updatePlot();
}

void Q3VxByYSettingsWidget::updatePlot()
{
    vXByYPlot_.setXValue(ui->xSpinBox->value());
    vXByYPlot_.update();

    if (plot_)
        plot_->update();
}
