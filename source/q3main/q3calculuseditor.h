#ifndef Q3CALCULUSEDITOR_H
#define Q3CALCULUSEDITOR_H

#include <QWidget>

#include "q3plot.h"
#include "q3mesh.h"
#include "q3calc.h"
#include "q3boundary.h"
#include "q3sceleton.h"
#include "q3directormanager.h"
#include "q3contour.h"

namespace Ui {
class Q3CalculusEditor;
}

class Q3CalculusEditor : public QWidget
{
    Q_OBJECT

public:
    explicit Q3CalculusEditor(Q3Plot *plot, Q3Mesh *mesh, QWidget *parent = 0);
    ~Q3CalculusEditor();

    void disable();
    void enable();

private slots:
    void updateInfo();
    void on_startCalcButton_clicked();
    void on_stopCalcButton_clicked();
    void on_resetCalcButton_clicked();

    void on_internalStreamPlotButton_clicked();
    void on_externalStreamPlotButton_clicked();

    void on_internalVorticityPlotButton_clicked();
    void on_externalVorticityPlotButton_clicked();

    void on_internalPreassurePlotButton_clicked();
    void on_externalPreassurePlotButton_clicked();

    void on_internalMagnitudePlotButton_clicked();
    void on_externalMagnitudePlotButton_clicked();

    void on_internalClearPlotButton_clicked();

private:
    Ui::Q3CalculusEditor *ui;

    Q3Mesh *mesh_;
    Q3Plot *plot_;
    Q3DirectorManager *directorManager_;
    Q3Calc *calc_;
    Q3ContourPlot contourPlot_;

    bool enabled_;
};

#endif // Q3CALCULUSEDITOR_H
