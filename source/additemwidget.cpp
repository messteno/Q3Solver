#include "additemdirector.h"
#include "additemwidget.h"

AddItemWidget::AddItemWidget(AddItemDirector *director, const QString &addButtonText)
{
    director_ = director;
    addButton_.setText(addButtonText);
    expanded_ = true;
}

QPushButton *AddItemWidget::getAddButton()
{
    return &addButton_;
}

void AddItemWidget::selected()
{
    director_->widgetButtonPushed(this);
}

void AddItemWidget::meshPlotClicked(QMeshPlot *meshPlot)
{

}

void AddItemWidget::clear()
{

}
