#include "q3sceleton.h"
#include "q3pointconnection.h"
#include "q3plot.h"

#include <QDebug>

Q3Sceleton::Q3Sceleton(QWidget *parent) :
    Q3PlotDrawable(parent)
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

Q3SceletonItem *Q3Sceleton::itemToResizeAt(const QPointF &pos, qreal radius) const
{
    qreal minDistance = 0;
    Q3SceletonItem *nearestItem = NULL;

    foreach (Q3SceletonItem *item, items_)
    {
        if (!item->isResizable())
            continue;

        qreal distance = item->distanceFromBoundaryTo(pos);
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

void Q3Sceleton::removeItem(Q3SceletonItem *item)
{
    if (item->type() == Q3SceletonItem::Point)
    {
        Q3Point *point = dynamic_cast<Q3Point *>(item);
        if (point)
        {
            foreach(Q3SceletonItem *testItem, items_)
            {
                Q3PointConnection *conn = dynamic_cast<Q3PointConnection *>(testItem);
                if (conn)
                {
                    if (conn->a() == item)
                        conn->setA(new Q3Point(*point));
                    if (conn->b() == item)
                        conn->setB(new Q3Point(*point));
                }
            }
        }
    }
    items_.removeAll(item);
    delete item;
}

void Q3Sceleton::removeSelectedItems()
{
    // We need to select all connected items to delete them too
    foreach (Q3SceletonItem *item, items_)
    {
        if (!item->isSelected())
            continue;
        switch (item->type())
        {
            case Q3SceletonItem::Point:
            {
                Q3Point *point = dynamic_cast<Q3Point *>(item);
                if (point)
                {
                    foreach (Q3SceletonItem *testItem, items_)
                    {
                        Q3PointConnection *conn = dynamic_cast<Q3PointConnection *>(testItem);
                        if (conn && (conn->a() == item || conn->b() == item))
                            conn->setSelected(true);
                    }
                }
                break;
            }
            default:
                break;
        }
    }

    QMutableListIterator<Q3SceletonItem *> it(items_);
    while(it.hasNext())
    {
        Q3SceletonItem *item = it.next();
        if (item->isSelected())
        {
            it.remove();
            delete item;
        }
    }
}

void Q3Sceleton::draw(Q3Painter &painter) const
{
    painter.save();
    QPen pen = painter.pen();
    foreach (Q3SceletonItem *item, items_)
    {
        painter.setBrush(item->backgroundColor());
        pen.setColor(item->penColor());
        painter.setPen(pen);
        item->draw(painter);
    }
    painter.restore();
}
