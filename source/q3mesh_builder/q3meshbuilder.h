#ifndef Q3MESHBUILDER_H
#define Q3MESHBUILDER_H

#include <QWidget>
#include <QKeyEvent>

#include "q3meshadapter.h"
#include "q3sceleton.h"
#include "q3director.h"
#include "q3plot.h"
#include "q3mesh.h"

namespace Ui {
class Q3MeshBuilder;
}

class Q3MeshBuilder : public QWidget
{
    Q_OBJECT

public:
    explicit Q3MeshBuilder(QWidget *parent = 0);
    ~Q3MeshBuilder();

    void keyReleaseEvent(QKeyEvent *event);

private slots:

    // in local coordinates
    void plotMouseClicked(QMouseEvent *event, const QPointF &scenePos);
    void plotMouseDragged(const QPointF &oldScenePos, const QPointF &newScenePos);
    void plotMouseDropped(const QPointF &scenePos);
    void plotMouseMoved(const QPointF &oldScenePos, const QPointF &newScenePos);

    void on_pointButton_clicked(bool checked);
    void on_pointConnectionButton_clicked(bool checked);
    void on_circleButton_clicked(bool checked);

    void on_createMeshButton_clicked();

    void on_removeMeshButton_clicked();

    void on_autoParameters_toggled(bool checked);

    void on_elementsCountSlider_valueChanged(int value);

    void on_elementsCountSpinBox_valueChanged(int arg1);

    void on_elementSizeSlider_valueChanged(int value);

    void on_elemetSizeSpinBox_valueChanged(double arg1);

private:
    QList<Q3Director *> directors_;

    Q3Sceleton *sceleton_;

    Q3Mesh *mesh_;
    Q3MeshAdapter *meshAdapter_;

    Ui::Q3MeshBuilder *ui;
};

#endif // Q3MESHBUILDER_H
