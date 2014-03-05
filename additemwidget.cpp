#include "additemdirector.h"
#include "additemwidget.h"

AddItemWidget::AddItemWidget(AddItemDirector *director, QWidget *parent) :
    QWidget(parent)
{
    director_ = director;
    expanded_ = true;
}

void AddItemWidget::expand()
{
    expanded_ = true;
}

void AddItemWidget::shrink()
{
    expanded_ = false;
}

void AddItemWidget::selected()
{
    director_->widgetButtonPushed(this);
}
