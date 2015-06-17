#ifndef Q3CONTOURSETTINGSWIDGET_H
#define Q3CONTOURSETTINGSWIDGET_H

#include <QWidget>
#include <QList>
#include <QStringListModel>
#include <QKeyEvent>

#include "q3plotsettingswidget.h"
#include "q3contour.h"

namespace Ui {
class Q3ContourSettingsWidget;
}

class Q3ContourSettingsWidget : public Q3PlotSettingsWidget
{
    Q_OBJECT

public:
    explicit Q3ContourSettingsWidget(Q3ContourPlot &contourPlot,
                                     QWidget *parent = 0);
    ~Q3ContourSettingsWidget();

private slots:
    void on_coloursCountSpinBox_valueChanged(int value);

private:
    Ui::Q3ContourSettingsWidget *ui;
    Q3ContourPlot &contourPlot_;
};

#endif // Q3CONTOURSETTINGSWIDGET_H
