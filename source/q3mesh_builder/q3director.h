#ifndef Q3DIRECTOR_H
#define Q3DIRECTOR_H

#include "q3plotdrawable.h"
#include "q3sceletonitem.h"

class Q3Plot;
class Q3Sceleton;

class Q3Director : public Q3PlotDrawable
{
public:
    enum Type
    {
        Move,
        Select,
        Add,
        Resize,
    };

protected:
    Q3SceletonItem::Type itemType_;
    static const int SelectRadius;
    bool active_;
    bool enabled_;
    Type type_;

public:
    Q3Director(Type type, QWidget *pranet = NULL);
    virtual ~Q3Director();

    Q3SceletonItem::Type itemType() const;
    Q3Director::Type type() const;

    virtual void setItemType(Q3SceletonItem::Type type);

    virtual bool processClick(Q3Plot *plot,
                              Q3Sceleton *sceleton,
                              const QPointF &scenePos,
                              bool snapToGrid);
    virtual bool processDragged(Q3Plot *plot,
                                Q3Sceleton *sceleton,
                                const QPointF &oldScenePos,
                                const QPointF &newScenePos,
                                bool snapToGrid);
    virtual bool processDropped(Q3Plot *plot,
                                Q3Sceleton *sceleton,
                                const QPointF &scenePos,
                                bool snapToGrid);
    virtual bool processMoved(Q3Plot *plot,
                              Q3Sceleton *sceleton,
                              const QPointF &oldScenePos,
                              const QPointF &newScenePos,
                              bool snapToGrid);
    virtual bool processKeyRelease(Q3Plot *plot,
                                   Q3Sceleton *sceleton,
                                   int key,
                                   bool snapToGrid);
    virtual void stop() = 0;

    virtual bool isActive() const;
    void setActive(bool active);

    bool isEnabled() const;
    void setEnabled(bool enabled);

    static QList<Q3Director *> orderListByActivity(
            const QList<Q3Director *> &directors);
};

#endif // Q3DIRECTOR_H
