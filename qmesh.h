#ifndef QMESH_H
#define QMESH_H

#include <QWidget>
#include "qmeshitem.h"
#include "qmeshrectitem.h"

class QMesh : public QWidget
{
    Q_OBJECT
public:
    explicit QMesh(QWidget *parent = 0);
    ~QMesh();

    void addItem(QMeshItem *item);

    void resizeEvent(QResizeEvent *event);
    void paintEvent(QPaintEvent *event);
    void wheelEvent(QWheelEvent *event);
    void mouseMoveEvent(QMouseEvent *event);
    void mouseReleaseEvent(QMouseEvent *);

signals:

public slots:

private:
    QList<QMeshItem *> items_;

    QRectF drawRect_;
    QRectF sceneRect_;
    qreal scaleX_;
    qreal scaleY_;
    qreal dx_;
    qreal dy_;

    QPointF mousePos_;

    QColor backgroundColor_;
    QColor foregroundColor_;


    void updateScene();

    QPointF sceneToMap(QPointF pos);
    QPointF sceneToMap(qreal x, qreal y);
    qreal sceneToMapX (qreal x);
    qreal sceneToMapY (qreal y);

    QPointF mapToScene(QPointF pos);
    QPointF mapToScene(qreal x, qreal y);
    qreal mapToSceneX (qreal x);
    qreal mapToSceneY (qreal y);

    void drawAxes();
    void drawItems();
};

#endif // QMESH_H
