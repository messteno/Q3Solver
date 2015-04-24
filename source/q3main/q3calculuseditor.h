#ifndef Q3CALCULUSEDITOR_H
#define Q3CALCULUSEDITOR_H

#include <QWidget>

#include "q3plot.h"
#include "q3mesh.h"
#include "q3calc.h"
#include "q3boundary.h"
#include "q3sceleton.h"
#include "q3directormanager.h"

namespace Ui {
class Q3CalculusEditor;
}

class Q3CalculusEditor : public QWidget
{
    Q_OBJECT

public:
    explicit Q3CalculusEditor(Q3Plot *plot, Q3Mesh *mesh, QWidget *parent = 0);
    ~Q3CalculusEditor();

private slots:
    void on_startCalculusButton_clicked();
    void on_stopalculusButton_clicked();
    void updateInfo();

private:
    Ui::Q3CalculusEditor *ui;

    Q3Mesh *mesh_;
    Q3DirectorManager *directorManager_;
    Q3Calc *calc_;
};

#endif // Q3CALCULUSEDITOR_H
