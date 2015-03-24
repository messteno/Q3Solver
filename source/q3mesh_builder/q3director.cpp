#include "q3director.h"

const int Q3Director::SelectRadius = 10;

Q3Director::Q3Director(Type type, QWidget *parent) :
    Q3PlotDrawable(),
    QWidget(parent),
    type_(type),
    itemType_(Q3SceletonItem::Base),
    enabled_(true),
    sceleton_(NULL),
    plot_(NULL),
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

bool Q3Director::processClick(QMouseEvent *event, const QPointF &scenePos, bool snapToGrid)
{
    return false;
}

bool Q3Director::processDragged(const QPointF &oldScenePos,
                                const QPointF &newScenePos, bool snapToGrid)
{
    return false;
}

bool Q3Director::processDropped(const QPointF &scenePos, bool snapToGrid)
{
    return false;
}

bool Q3Director::processMoved(const QPointF &oldScenePos,
                              const QPointF &newScenePos,
                              bool snapToGrid)
{
    return false;
}

bool Q3Director::processKeyRelease(int key, bool snapToGrid)
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

void Q3Director::setSceleton(Q3Sceleton *sceleton)
{
    sceleton_ = sceleton;
}

void Q3Director::setPlot(Q3Plot *plot)
{
    plot_ = plot;
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
