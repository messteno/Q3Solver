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

    void setBackgroundColor(const QColor &color);
    void setForegroundColor(const QColor &color);
    void setPenColor(const QColor &color);
    void setAxesColor(const QColor &color);

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
    QColor penColor_;
    QColor axesColor_;

    void updateScene();

    QPointF sceneToMap(const QPointF &pos) const;
    QPointF sceneToMap(qreal x, qreal y) const;
    qreal sceneToMapX (qreal x) const;
    qreal sceneToMapY (qreal y) const;

    QPointF mapToScene(const QPointF &pos) const;
    QPointF mapToScene(qreal x, qreal y) const;
    qreal mapToSceneX (qreal x) const;
    qreal mapToSceneY (qreal y) const;

    void drawBackground();
    void drawAxes();
    void drawItems();
};

#endif // QMESH_H
