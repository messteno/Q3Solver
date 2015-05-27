#include <QDebug>

#include "q3boundary.h"

Q3Boundary::Q3Boundary() :
    item_(NULL),
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
    return item == item_;
}

void Q3Boundary::addItem(Q3SceletonItem *item)
{
    Q_ASSERT(!item_);
    item_ = item;
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

QVector2D Q3Boundary::velocity(QPointF a, QPointF b)
{
    Q_ASSERT(type_);
    // TODO: Возможно нужно проверить на принадлежность точки границе
    return type_->velocity(item_, a, b);
}

void Q3Boundary::setType(Q3BoundaryType *type)
{
    type_ = type;
}

Q3Boundary* Q3Boundary::findByElement(QList<Q3Boundary *> *boundaries,
                                     Q3SceletonItem *item)
{
    foreach(Q3Boundary *boundary, *boundaries)
    {
        if (boundary->contains(item))
            return boundary;
    }
    return NULL;
}

Q3Boundary *Q3Boundary::findByLabel(QList<Q3Boundary *> *boundaries, int label)
{
    foreach(Q3Boundary *boundary, *boundaries)
    {
        if (boundary->label() == label)
            return boundary;
    }
    return NULL;
}

void Q3Boundary::setUniqueLabels(QList<Q3Boundary *> *boundaries)
{
    int label = 1;
    foreach (Q3Boundary *boundary, *boundaries)
        boundary->setLabel(label++);
}
