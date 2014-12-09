#ifndef Q3PLOT_H
#define Q3PLOT_H

#include <QWidget>

class Q3Plot : public QWidget
{
    Q_OBJECT
public:
    explicit Q3Plot(QWidget *parent = 0);
    ~Q3Plot();

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

    QPointF getClickedScenePosition(bool snapToGrid);

signals:
    void mouseClicked(Q3Plot *meshPlot);

public slots:

private:
    QRectF sceneRect_;
    QRectF drawRect_;
    qreal scaleX_;
    qreal scaleY_;
    qreal dx_;
    qreal dy_;

    double tickDx_;
    double tickDy_;
    int countTickX_;
    int countTickY_;

    QPointF mousePos_;
    QPointF clickedPos_;

    QColor backgroundColor_;
    QColor foregroundColor_;
    QColor penColor_;
    QColor axesColor_;

    int bottomMargin_;
    int leftMargin_;

    int wheelDelta_;

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
};

#endif // Q3PLOT_H
