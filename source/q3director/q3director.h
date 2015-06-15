#ifndef Q3DIRECTOR_H
#define Q3DIRECTOR_H

#include <QMouseEvent>

#include "q3plotdrawable.h"
#include "q3sceletonitem.h"

class Q3Plot;
class Q3Sceleton;

class Q3Director : public QWidget, public Q3PlotDrawable
{
    Q_OBJECT

public:
    enum Type
    {
        Move,
        Select,
        Add,
        Resize,
        Boundary,
        PointInfo,
        Contour,
    };

protected:
    Q3SceletonItem::Type itemType_;
    Q3Sceleton *sceleton_;
    Q3Plot *plot_;
    static const int SelectRadius;
    bool active_;
    bool enabled_;
    Type type_;

public:
    Q3Director(Type type, QWidget *parent = NULL);
    virtual ~Q3Director();

    Q3SceletonItem::Type itemType() const;
    Q3Director::Type type() const;

    virtual void setItemType(Q3SceletonItem::Type type);

    virtual bool processClick(QMouseEvent *event, const QPointF &scenePos);
    virtual bool processDragged(const QPointF &oldScenePos,
                                const QPointF &newScenePos);
    virtual bool processDropped(const QPointF &scenePos);
    virtual bool processMoved(const QPointF &oldScenePos,
                              const QPointF &newScenePos);
    virtual bool processKeyRelease(int key);
    virtual void stop();

    virtual bool isActive() const;
    void setActive(bool active);

    bool isEnabled() const;
    void setEnabled(bool enabled);

    static QList<Q3Director *> orderListByActivity(
            const QList<Q3Director *> &directors);

    void setSceleton(Q3Sceleton *sceleton);
    void setPlot(Q3Plot *plot);
};

#endif // Q3DIRECTOR_H
