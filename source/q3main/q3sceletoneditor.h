#ifndef Q3MESHBUILDER_H
#define Q3MESHBUILDER_H

#include <QWidget>
#include <QKeyEvent>

#include "q3meshadapter.h"
#include "q3sceleton.h"
#include "q3directormanager.h"
#include "q3plot.h"
#include "q3mesh.h"

namespace Ui {
class Q3MeshBuilder;
}

class Q3SceletonEditor : public QWidget
{
    Q_OBJECT

public:
    explicit Q3SceletonEditor(Q3Plot *plot, Q3Sceleton &sceleton,
                              QWidget *parent = 0);
    ~Q3SceletonEditor();

private slots:

    void on_pointButton_clicked(bool checked);
    void on_pointConnectionButton_clicked(bool checked);
    void on_circleButton_clicked(bool checked);

    void on_prepareSceletonButton_clicked();

    void on_snapToGrid_toggled(bool checked);

    void on_saveSceletonButton_clicked();

signals:
    void goToTab(int tab);

private:
    Q3Sceleton &sceleton_;

    Q3Mesh *mesh_;
    Q3MeshAdapter *meshAdapter_;
    Q3Plot *plot_;

    Q3DirectorManager *directorManager_;

    Ui::Q3MeshBuilder *ui;
};

#endif // Q3MESHBUILDER_H
