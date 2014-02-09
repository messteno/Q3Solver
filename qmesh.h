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
    //void mouseMoveEvent(QMoveEvent *event);
    //void mouseReleaseEvent(QMouseEvent *event);

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

    QColor backgroundColor_;
    QColor foregroundColor_;

    QPointF mousePosition_;


    void updateScene();
    qreal sceneToMapX (qreal x);
    qreal sceneToMapY (qreal y);

    void drawAxes();
    void drawItems();
};

#endif // QMESH_H
