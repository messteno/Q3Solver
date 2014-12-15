#include "q3sceletonitem.h"
#include "q3plot.h"

const QColor Q3SceletonItem::DefaultColor =
        QColor(Q3Plot::DefaultForegroundColor);
const QColor Q3SceletonItem::SelectedColor = QColor(Qt::red);

Q3SceletonItem::Q3SceletonItem(Type type) :
    selected_(false),
    selectable_(true),
    color_(DefaultColor),
    selectedColor_(SelectedColor),
    type_(type)
{

}

Q3SceletonItem::~Q3SceletonItem()
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

Q3SceletonItem::Type Q3SceletonItem::type()
{
    return type_;
}

void Q3SceletonItem::setColor(const QColor &color)
{
    color_ = color;
}

void Q3SceletonItem::setSelectedColor(const QColor &color)
{
    selectedColor_ = color;
}

const QColor &Q3SceletonItem::color() const
{
    if (selected_)
        return selectedColor_;
    return color_;
}
