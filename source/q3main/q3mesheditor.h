#ifndef Q3MESHEDITOR_H
#define Q3MESHEDITOR_H

#include <QWidget>

#include "q3plot.h"
#include "q3sceleton.h"
#include "q3mesh.h"
#include "q3boundary.h"
#include "q3meshadapter.h"
#include "q3directormanager.h"

namespace Ui {
class Q3MeshEditor;
}

class Q3MeshEditor : public QWidget
{
    Q_OBJECT

public:
    explicit Q3MeshEditor(Q3Plot *plot, Q3Mesh *mesh, Q3Sceleton *sceleton,
                          QList<Q3Boundary *> *boundaries, QWidget *parent = 0);
    ~Q3MeshEditor();

    void disable();
    void enable();

private slots:
    void on_createMeshButton_clicked();
    void on_meshParameter_count_clicked();
    void on_meshParameter_size_clicked();
    void on_meshParameter_auto_clicked();
    void on_elementsCountSlider_valueChanged(int value);
    void on_elementsCountSpinBox_valueChanged(int arg1);
    void on_elementSizeSlider_valueChanged(int value);
    void on_elementSizeSpinBox_valueChanged(double arg1);
    void on_removeMeshButton_clicked();
    void on_saveMeshButton_clicked();

signals:
    void goToTab(int tab);

private:
    Ui::Q3MeshEditor *ui;

    Q3Plot *plot_;
    Q3Mesh *mesh_;
    QList<Q3Boundary *> *boundaries_;
    Q3Sceleton *sceleton_;
    Q3MeshAdapter *meshAdapter_;
    Q3DirectorManager *directorManager_;

    bool enabled_;
};

#endif // Q3MESHEDITOR_H
