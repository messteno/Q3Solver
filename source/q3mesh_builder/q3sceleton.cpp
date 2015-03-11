#include <QtAlgorithms>
#include <QDebug>
#include <QMessageBox>
#include <QtGlobal>
#include <QTime>

#include "q3sceleton.h"
#include "q3pointconnection.h"
#include "q3plot.h"
#include "q3itemvisitor.h"

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

Q3SceletonItem *Q3Sceleton::itemToResizeAt(const QPointF &pos,
                                           qreal radius) const
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
                Q3PointConnection *conn = \
                        dynamic_cast<Q3PointConnection *>(testItem);
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
                        Q3PointConnection *conn = \
                                dynamic_cast<Q3PointConnection *>(testItem);
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

bool Q3Sceleton::createMesh(Q3MeshAdapter *adapter)
{
    outerBoundary_.clear();
    innerBoundaries_.clear();

    if (!items_.count())
    {
        QMessageBox::warning(this, tr("Q3Solver"),
                             tr("Невозможно создать сетку, "
                                "элементы отсутствуют"),
                             QMessageBox::Ok);
        return false;
    }

    // Проверить отсутствие пересечения элементов
    Q3ItemCrossVisitor crossVisitor;
    foreach (Q3SceletonItem *item1, items_)
    {
        foreach (Q3SceletonItem *item2, items_)
        {
            if (item1 == item2)
                continue;

            bool crosses = item1->accept(crossVisitor, item2);
            if (crosses)
            {
                QMessageBox::warning(this, tr("Q3Solver"),
                                     tr("Невозможно создать сетку, "
                                        "некоторые элементы пересекаются"),
                                     QMessageBox::Ok);
                return false;
            }
        }
    }

    // Найти самый левый элемент
    Q3ItemLeftmostVisitor leftmostVisitor;
    foreach (Q3SceletonItem *item, items_)
        item->accept(leftmostVisitor);
    Q3SceletonItem *leftmost = leftmostVisitor.leftmost();
    Q_ASSERT(leftmost);

    // Найти все связанные элементы
    // Проверить, что получилась замкнутая область
    Q3ItemConnectedVisitor connectedVisitor;
    leftmost->accept(connectedVisitor);
    while (!connectedVisitor.isFinished())
    {
        bool found = false;
        foreach (Q3SceletonItem *item, items_)
        {
            if (item->accept(connectedVisitor))
            {
                found = true;
                break;
            }
        }

        // Если не нашли нового соединения, попробуем сделать шаг назад
        if (!found)
            connectedVisitor.backward();
    }

    outerBoundary_ = connectedVisitor.connectedItems();

    if (outerBoundary_.empty())
    {
        QMessageBox::warning(this, tr("Q3Solver"),
                             tr("Невозможно создать сетку, "
                                "внешняя область не замкнута"),
                             QMessageBox::Ok);
        return false;
    }

    // TODO: проверить, что из каждой точки внешней границы выходит ровно два
    //       отрезка границы

    // Проверить, что все остальные элементы внутри выделенной области
    foreach (Q3SceletonItem *item, items_)
    {
        if (outerBoundary_.contains(item))
            continue;

        Q3ItemRayTraceVisitor rayTraceVisitor(item);
        foreach (Q3SceletonItem *chainItem, outerBoundary_)
            chainItem->accept(rayTraceVisitor);

        if (rayTraceVisitor.rayCrossCount() % 2 == 0)
        {
            QMessageBox::warning(this, tr("Q3Solver"),
                                 tr("Невозможно создать сетку, "
                                    "внешняя область не односвязна"),
                                 QMessageBox::Ok);
            return false;
        }
    }

    // TODO: возможно, переписать то, что выше =)

    // Сделать копию списка элементов
    // Убрать отрезки внешней границы
    QList<Q3SceletonItem *> activeItems = items_;
    foreach (Q3SceletonItem *item, outerBoundary_)
        activeItems.removeAll(item);

    // Сортируем элементы по оси OX
    QList<Q3SceletonItem *> leftOrderItems = items_;
    qSort(leftOrderItems.begin(), leftOrderItems.end(),
          Q3SceletonItem::lefterThan);

    // Ищем внутренние замкнутые границы
    foreach (Q3SceletonItem *item, leftOrderItems)
    {
        Q3ItemInnerBoundaryVisitor innerBoundaryVisitor(activeItems);
        bool found = item->accept(innerBoundaryVisitor);
        if (found)
        {
            QList<Q3SceletonItem *> boundary = \
                    innerBoundaryVisitor.getBoundary();
            foreach (Q3SceletonItem *item, boundary)
                activeItems.removeAll(item);
            innerBoundaries_.append(boundary);
        }
    }

    // Проверяем, что внутри внутренних областей нет других элементов
    QList<QList<Q3SceletonItem *> >::iterator it;
    for (it = innerBoundaries_.begin(); it != innerBoundaries_.end(); ++it)
    {
        foreach (Q3SceletonItem *item, items_)
        {
            if ((*it).contains(item))
                continue;

            Q3ItemRayTraceVisitor rayTraceVisitor(item);
            foreach (Q3SceletonItem *chainItem, *it)
                chainItem->accept(rayTraceVisitor);

            if (rayTraceVisitor.rayCrossCount() % 2 == 1)
            {
                QMessageBox::warning(this, tr("Q3Solver"),
                                     tr("Невозможно создать сетку, "
                                        "внутренняя обасть содержит элементы"),
                                     QMessageBox::Ok);
                return false;
            }
        }
    }

    adapter->generateMesh(items_,
                          outerBoundary_,
                          innerBoundaries_,
                          activeItems);

//    qsrand(QTime::currentTime().msec());
//    QColor color(qrand() % 0xff, qrand() % 0xff, qrand() %0xff);
//    foreach (Q3SceletonItem *item, outerBoundary_)
//    {
//        item->setBackgroundColor(color);
//        item->setPenColor(color);
//    }

//    for (it = innerBoundaries_.begin(); it != innerBoundaries_.end(); ++it)
//    {
//        QColor color(qrand() % 0xff, qrand() % 0xff, qrand() %0xff);
//        foreach (Q3SceletonItem *item, *it)
//        {
//            item->setBackgroundColor(color);
//            item->setPenColor(color);
//        }
//    }

    return true;
}
