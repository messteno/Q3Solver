#ifndef Q3MESHBUILDER_H
#define Q3MESHBUILDER_H

#include <QDialog>
#include <QKeyEvent>

#include "q3meshadapter.h"
#include "q3sceleton.h"
#include "q3directormanager.h"
#include "q3plot.h"
#include "q3mesh.h"

namespace Ui {
class Q3MeshBuilder;
}

class Q3MeshBuilder : public QDialog
{
    Q_OBJECT

public:
    explicit Q3MeshBuilder(Q3Mesh *mesh, Q3Sceleton *sceleton,
                           QWidget *parent = 0);
    ~Q3MeshBuilder();

private slots:

    void on_pointButton_clicked(bool checked);
    void on_pointConnectionButton_clicked(bool checked);
    void on_circleButton_clicked(bool checked);

    void on_createMeshButton_clicked();
    void on_removeMeshButton_clicked();

    void on_autoParameters_toggled(bool checked);

    void on_elementsCountSlider_valueChanged(int value);
    void on_elementsCountSpinBox_valueChanged(int arg1);
    void on_elementSizeSlider_valueChanged(int value);
    void on_elementSizeSpinBox_valueChanged(double arg1);

    void on_snapToGrid_toggled(bool checked);

private:
    Q3Sceleton *sceleton_;

    Q3Mesh *mesh_;
    Q3MeshAdapter *meshAdapter_;

    Q3DirectorManager *directorManager_;

    Ui::Q3MeshBuilder *ui;
};

#endif // Q3MESHBUILDER_H
