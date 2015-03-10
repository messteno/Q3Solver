#ifndef Q3MESHBUILDER_H
#define Q3MESHBUILDER_H

#include <QWidget>
#include <QKeyEvent>

#include "q3sceleton.h"
#include "q3director.h"
#include "q3plot.h"

namespace Ui {
class Q3MeshBuilder;
}

class Q3MeshBuilder : public QWidget
{
    Q_OBJECT

public:
    static const int SelectRadius;

    enum ProcessMode
    {
        NoProcess,
        MoveProcess,
        MoveItemProcess,
        AddItemProcess,
    };

    explicit Q3MeshBuilder(QWidget *parent = 0);
    ~Q3MeshBuilder();

    void keyReleaseEvent(QKeyEvent *event);

private slots:

    // in local coordinates
    void plotMouseClicked(const QPointF &scenePos);
    void plotMouseDragged(const QPointF &oldScenePos, const QPointF &newScenePos);
    void plotMouseDropped(const QPointF &scenePos);
    void plotMouseMoved(const QPointF &oldScenePos, const QPointF &newScenePos);

    void on_pointButton_clicked(bool checked);
    void on_pointConnectionButton_clicked(bool checked);
    void on_circleButton_clicked(bool checked);

    void on_createMeshButton_clicked();

private:
    Q3Sceleton *sceleton_;
    QList<Q3Director *> directors_;

    Ui::Q3MeshBuilder *ui;
};

#endif // Q3MESHBUILDER_H
