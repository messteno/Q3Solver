#ifndef Q3PLOT_H
#define Q3PLOT_H

#include <QWidget>

#include <q3sceleton.h>

class Q3Plot : public QWidget
{
    Q_OBJECT

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

    QColor backgroundColor_;
    QColor foregroundColor_;
    QColor penColor_;
    QColor axesColor_;

    int bottomMargin_;
    int leftMargin_;

    int wheelDelta_;

    Q3Sceleton *sceleton_;

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
    void drawSceleton();

public:
    static const QColor DefaultBackgroundColor;
    static const QColor DefaultForegroundColor;
    static const QColor DefaultAxesColor;
    static const int MinTickCount;

    explicit Q3Plot(QWidget *parent = 0);
    ~Q3Plot();

    void resizeEvent(QResizeEvent *event);
    void paintEvent(QPaintEvent *event);
    void wheelEvent(QWheelEvent *event);
    void mouseMoveEvent(QMouseEvent *event);
    void mouseReleaseEvent(QMouseEvent *);

    qreal sx() const;
    qreal sy() const;

    void moveScene(const QPointF diff);

    void setBackgroundColor(const QColor &color);
    void setForegroundColor(const QColor &color);
    void setPenColor(const QColor &color);
    void setAxesColor(const QColor &color);
    void setBottomMargin(int margin);
    void setLeftMargin(int margin);

    void setSceleton(Q3Sceleton *sceleton);

    QPointF snapScenePosToGrid (const QPointF pos);

signals:
    void mouseClicked(const QPointF scenePos);
    void mouseDragged(const QPointF oldScenePos, const QPointF newScenePos);
    void mouseDropped(const QPointF scenePos);

public slots:
};

#endif // Q3PLOT_H
