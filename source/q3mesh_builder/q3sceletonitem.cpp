#include "q3sceletonitem.h"
#include "q3plot.h"

const QColor Q3SceletonItem::BackgroundColor =
        QColor(Q3Plot::DefaultForegroundColor);
const QColor Q3SceletonItem::SelectedBackgroundColor = QColor(Qt::red);
const QColor Q3SceletonItem::PenColor =
        QColor(Q3Plot::DefaultPenColor);
const QColor Q3SceletonItem::SelectedPenColor = QColor(Qt::lightGray);

Q3SceletonItem::Q3SceletonItem(Type type) :
    selected_(false),
    selectable_(true),
    resizing_(false),
    resizable_(false),
    backgroundColor_(BackgroundColor),
    selectedBackgroundColor_(SelectedBackgroundColor),
    penColor_(PenColor),
    selectedPenColor_(SelectedPenColor),
    type_(type)
{

}

Q3SceletonItem::~Q3SceletonItem()
{

}

void Q3SceletonItem::resize(const QPointF from, const QPointF to)
{

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
