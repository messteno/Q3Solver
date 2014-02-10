#ifndef QMESHPLOT_H
#define QMESHPLOT_H

#include <QWidget>
#include "qmeshitem.h"
#include "qmeshrectitem.h"

class QMeshPlot : public QWidget
{
    Q_OBJECT
public:
    explicit QMeshPlot(QWidget *parent = 0);
    ~QMeshPlot();

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
    void setBottomMargin(int margin);
    void setLeftMargin(int margin);

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

    double tickDx_;
    double tickDy_;
    int countTickX_;
    int countTickY_;

    QPointF mousePos_;

    QColor backgroundColor_;
    QColor foregroundColor_;
    QColor penColor_;
    QColor axesColor_;

    int bottomMargin_;
    int leftMargin_;

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
    void drawBorders();
    void drawItems();
};

#endif // QMESHPLOT_H
