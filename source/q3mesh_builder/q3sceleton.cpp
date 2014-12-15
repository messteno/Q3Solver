#include "q3sceleton.h"
#include "q3plot.h"

#include <QDebug>

Q3Sceleton::Q3Sceleton(QWidget *parent) :
    QWidget(parent)
{
}

Q3Sceleton::~Q3Sceleton()
{

}

Q3SceletonItem* Q3Sceleton::itemAt(const QPointF &pos, qreal radius,
                                  Q3SceletonItem::Type type) const
{
    qreal minDistance = 0;
    Q3SceletonItem *nearestItem = NULL;

    foreach (Q3SceletonItem *item, items_)
    {
        if (type != Q3SceletonItem::Base && item->type() != type)
            continue;

        qreal distance = item->distanceTo(pos);
        if (distance > radius)
            continue;

        if (!nearestItem)
        {
            minDistance = distance;
            nearestItem = item;
        }
        else if (item->type() != Q3SceletonItem::PointConnection &&
                 distance < minDistance)
        {
            minDistance = distance;
            nearestItem = item;
        }
    }
    return nearestItem;
}

void Q3Sceleton::addItem(Q3SceletonItem *item)
{
    items_.append(item);
}

void Q3Sceleton::drawItems(Q3Painter &painter) const
{
    painter.save();
    foreach (Q3SceletonItem *item, items_)
    {
        painter.setBrush(item->color());
        item->draw(painter);
    }
    painter.restore();
}
