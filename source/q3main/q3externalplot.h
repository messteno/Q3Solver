#ifndef Q3EXTERNALPLOT_H
#define Q3EXTERNALPLOT_H

#include <QDialog>
#include "q3plotdrawable.h"
#include "q3directormanager.h"
#include "q3plotsettingswidget.h"

namespace Ui {
class Q3ExternalPlot;
}

class Q3ExternalPlot : public QDialog
{
    Q_OBJECT

public:
    explicit Q3ExternalPlot(QWidget *parent = 0);
    ~Q3ExternalPlot();
    Q3Plot* plotWidget();

    //  Добавляет динамический объект, который будет удален в деструкторе
    void addDrawable(Q3PlotDrawable *drawable);
    void addSettingsWidget(Q3PlotSettingsWidget *widget);

private slots:
    void on_closeButton_clicked();
    void on_savePlotButton_clicked();

private:
    Ui::Q3ExternalPlot *ui;
    Q3DirectorManager *directorManager_;
    QList<Q3PlotDrawable *> drawables_;
};

#endif // Q3EXTERNALPLOT_H
