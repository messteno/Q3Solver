#ifndef Q3VYBYXSETTINGSWIDGET_H
#define Q3VYBYXSETTINGSWIDGET_H

#include <QWidget>
#include "q3plotsettingswidget.h"
#include "q3graphs.h"

namespace Ui {
class Q3VyByXSettingsWidget;
}

class Q3VyByXSettingsWidget : public Q3PlotSettingsWidget
{
    Q_OBJECT

public:
    explicit Q3VyByXSettingsWidget(Q3VyByXPlot &vYByXPlot, qreal minY, qreal maxY,
                                   QWidget *parent = 0);
    ~Q3VyByXSettingsWidget();

private slots:
    void on_ySpinBox_valueChanged(qreal value);

private:
    Ui::Q3VyByXSettingsWidget *ui;
    Q3VyByXPlot &vYByXPlot_;
    qreal minY_;
    qreal maxY_;

    void updatePlot();
};

#endif // Q3VYBYXSETTINGSWIDGET_H
