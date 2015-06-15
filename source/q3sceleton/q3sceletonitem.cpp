#include <QDebug>

#include "q3sceletonitem.h"
#include "q3plot.h"
#include "q3itemvisitor.h"

Q3SceletonItem::Q3SceletonItem(Type type) :
    type_(type),
    moved_(false),
    selected_(false),
    selectable_(true),
    resizing_(false),
    resizable_(false),
    backgroundColor_(Q3Plot::DefaultForegroundColor),
    selectedBackgroundColor_(Qt::red),
    penColor_(Q3Plot::DefaultPenColor),
    selectedPenColor_(QColor(100, 100, 100))
{
}

Q3SceletonItem::~Q3SceletonItem()
{

}

void Q3SceletonItem::resize(const QPointF from, const QPointF to)
{
    Q_UNUSED(from);
    Q_UNUSED(to);
}

bool Q3SceletonItem::isSelectable() const
{
    return selectable_;
}

void Q3SceletonItem::setSelectable(bool selectable)
{
    selectable_ = selectable;
    if (!selectable)
        selected_ = false;
}

bool Q3SceletonItem::isSelected() const
{
    return selected_;
}

void Q3SceletonItem::setSelected(bool selected)
{
    if (selectable_)
        selected_ = selected;
    else
        selected_ = false;
}

bool Q3SceletonItem::isResizable() const
{
    return resizable_;
}

void Q3SceletonItem::setResizable(bool resizable)
{
    resizable_ = resizable;
    if (!resizable)
        resizing_ = false;
}

bool Q3SceletonItem::isResizing() const
{
    return resizing_;
}

void Q3SceletonItem::setResizing(bool resizing)
{
    if (resizable_)
        resizing_ = resizing;
    else
        resizing_ = false;
}

Q3SceletonItem::Type Q3SceletonItem::type()
{
    return type_;
}

void Q3SceletonItem::setBackgroundColor(const QColor &color)
{
    backgroundColor_ = color;
}

void Q3SceletonItem::setSelectedBackgroundColor(const QColor &color)
{
    selectedBackgroundColor_ = color;
}

void Q3SceletonItem::setPenColor(const QColor &penColor)
{
    penColor_ = penColor;
}

const QColor &Q3SceletonItem::backgroundColor() const
{
    if (selected_)
        return selectedBackgroundColor_;
    return backgroundColor_;
}

const QColor &Q3SceletonItem::penColor() const
{
    if (selected_)
        return selectedPenColor_;
    return penColor_;
}

bool Q3SceletonItem::lefterThan(const Q3SceletonItem *i1,
                                const Q3SceletonItem *i2)
{
    return i1->boundingRect().left() < i2->boundingRect().left();
}
bool Q3SceletonItem::moved() const
{
    return moved_;
}

void Q3SceletonItem::setMoved(bool moved)
{
    moved_ = moved;
}
