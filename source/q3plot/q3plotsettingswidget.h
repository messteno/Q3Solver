#ifndef Q3PLOTSETTINGSWIDGET_H
#define Q3PLOTSETTINGSWIDGET_H

#include <QWidget>
#include "q3plot.h"

class Q3PlotSettingsWidget : public QWidget
{
    Q_OBJECT
public:
    explicit Q3PlotSettingsWidget(QWidget *parent = 0);
    void setPlot(Q3Plot *plot);

signals:

public slots:

protected:
    Q3Plot *plot_;
};

#endif // Q3PLOTSETTINGSWIDGET_H
