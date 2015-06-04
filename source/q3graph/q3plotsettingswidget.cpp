#include "q3plotsettingswidget.h"

Q3PlotSettingsWidget::Q3PlotSettingsWidget(QWidget *parent) :
    QWidget(parent),
    plot_(NULL)
{
}

void Q3PlotSettingsWidget::setPlot(Q3Plot *plot)
{
    plot_ = plot;
}

