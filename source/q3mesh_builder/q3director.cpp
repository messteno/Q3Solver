#include "q3director.h"

const int Q3Director::SelectRadius = 10;

Q3Director::Q3Director(Type type, QWidget *pranet) :
    Q3PlotDrawable(pranet),
    type_(type),
    itemType_(Q3SceletonItem::Base),
    enabled_(true),
    active_(false)
{

}

Q3Director::~Q3Director()
{

}

Q3SceletonItem::Type Q3Director::itemType() const
{
    return itemType_;
}

Q3Director::Type Q3Director::type() const
{
    return type_;
}

void Q3Director::setItemType(Q3SceletonItem::Type type)
{
    if (itemType_ != type)
        stop();
    itemType_ = type;
}

bool Q3Director::processClick(Q3Plot *plot, Q3Sceleton *sceleton,
                              const QPointF &scenePos, bool snapToGrid)
{
    return false;
}

bool Q3Director::processDragged(Q3Plot *plot, Q3Sceleton *sceleton,
                                const QPointF &oldScenePos,
                                const QPointF &newScenePos, bool snapToGrid)
{
    return false;
}

bool Q3Director::processDropped(Q3Plot *plot, Q3Sceleton *sceleton,
                                const QPointF &scenePos, bool snapToGrid)
{
    return false;
}

bool Q3Director::processMoved(Q3Plot *plot, Q3Sceleton *sceleton,
                              const QPointF &oldScenePos,
                              const QPointF &newScenePos,
                              bool snapToGrid)
{
    return false;
}

bool Q3Director::processKeyRelease(Q3Plot *plot, Q3Sceleton *sceleton,
                                   int key, bool snapToGrid)
{
    return false;
}

bool Q3Director::isActive() const
{
    return active_;
}

void Q3Director::setActive(bool active)
{
    active_ = active;
}

bool Q3Director::isEnabled() const
{
    return enabled_;
}

void Q3Director::setEnabled(bool enabled)
{
    enabled_ = enabled;
}

QList<Q3Director *> Q3Director::orderListByActivity(
        const QList<Q3Director *> &directors)
{
    QList<Q3Director *> orderedByActivityDirectors;
    foreach (Q3Director *director, directors)
    {
        if (director->isActive())
            orderedByActivityDirectors.push_front(director);
        else
            orderedByActivityDirectors.push_back(director);
    }
    return orderedByActivityDirectors;
}
