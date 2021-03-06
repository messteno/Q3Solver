#include <QDebug>

#include "q3boundary.h"

Q3Boundary::Q3Boundary() :
    type_(NULL),
    label_(0)
{

}

Q3Boundary::~Q3Boundary()
{
    delete type_;
}

void Q3Boundary::setTypeByEnum(Q3BoundaryType::Type type)
{
    delete type_;
    type_ = Q3BoundaryType::getBoundaryTypeByEnum(type);
}

bool Q3Boundary::contains(Q3SceletonItem *item)
{
    return items_.contains(item);
}

void Q3Boundary::addItem(Q3SceletonItem *item)
{
    items_.append(item);
}

Q3BoundaryType *Q3Boundary::type() const
{
    return type_;
}

int Q3Boundary::label() const
{
    return label_;
}

void Q3Boundary::setLabel(int label)
{
    label_ = label;
}

QVector2D Q3Boundary::velocity(const QPointF &a, const QPointF &b, qreal time)
{
    // Тут нужно все переделать
    Q_ASSERT(type_);

    qreal minDist = 0;
    Q3SceletonItem *nearestItem = NULL;
    foreach (Q3SceletonItem *item, items_)
    {
        qreal dist = item->distanceFromBoundaryTo(a);
        if (!nearestItem || (dist < minDist))
        {
            minDist = dist;
            nearestItem = item;
        }
    }

    return type_->velocity(nearestItem, a, b, time);
}

QList<Q3SceletonItem *> Q3Boundary::items() const
{
    return items_;
}

void Q3Boundary::setType(Q3BoundaryType *type)
{
    type_ = type;
}

Q3Boundary* Q3Boundary::findByElement(QList<Q3Boundary *> &boundaries,
                                      Q3SceletonItem *item)
{
    foreach(Q3Boundary *boundary, boundaries)
    {
        if (boundary->contains(item))
            return boundary;
    }
    return NULL;
}

Q3Boundary *Q3Boundary::findByLabel(QList<Q3Boundary *> &boundaries, int label)
{
    foreach(Q3Boundary *boundary, boundaries)
    {
        if (boundary->label() == label)
            return boundary;
    }
    return NULL;
}

void Q3Boundary::setUniqueLabels(QList<Q3Boundary *> &boundaries)
{
    int label = 1;
    foreach (Q3Boundary *boundary, boundaries)
        boundary->setLabel(label++);
}
