#ifndef Q3VXBYYSETTINGSWIDGET_H
#define Q3VXBYYSETTINGSWIDGET_H

#include <QWidget>
#include "q3plotsettingswidget.h"
#include "q3graphs.h"

namespace Ui {
class Q3VxByYSettingsWidget;
}

class Q3VxByYSettingsWidget : public Q3PlotSettingsWidget
{
    Q_OBJECT

public:
    explicit Q3VxByYSettingsWidget(Q3VxByYPlot &vXByYPlot, qreal minX, qreal maxX,
                                   QWidget *parent = 0);
    ~Q3VxByYSettingsWidget();

private slots:
    void on_xSpinBox_valueChanged(qreal value);

private:
    Ui::Q3VxByYSettingsWidget *ui;
    Q3VxByYPlot &vXByYPlot_;
    qreal minX_;
    qreal maxX_;

    void updatePlot();
};

#endif // Q3VXBYYSETTINGSWIDGET_H
