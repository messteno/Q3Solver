#include "pointadditemwidget.h"
#include "lineadditemwidget.h"
#include "additemdirector.h"
#include "additemdirectorstateitemselect.h"
#include "qmeshplot.h"

#include <QVBoxLayout>

AddItemDirector::AddItemDirector(QMeshPlot *meshPlot, QWidget *parent) :
    QWidget(parent)
{
    meshPlot_ = meshPlot;
    state_ = AddItemDirectorStateItemSelect::getInstance();
    createWidgets();
    hide();
}

AddItemDirector::~AddItemDirector()
{
}

void AddItemDirector::widgetButtonPushed(AddItemWidget *selectedWidget)
{
    state_->widgetButtonPushed (this, selectedWidget);
}

void AddItemDirector::processWidgetSelected (AddItemWidget *selectedWidget)
{
    foreach (AddItemWidget *widget, widgets_)
    {
        if (selectedWidget == widget)
            widget->expand();
        else
        {
            widget->shrink();
            widget->hide();
        }
    }
}

void AddItemDirector::show()
{
    state_ = AddItemDirectorStateItemSelect::getInstance();
    QWidget::show();
    foreach (AddItemWidget *widget, widgets_)
    {
        widget->shrink();
        widget->show();
    }
}

void AddItemDirector::hide()
{
    state_ = AddItemDirectorStateItemSelect::getInstance();
    QWidget::hide();
}

void AddItemDirector::addItem(QMeshItem *item)
{
    meshPlot_->addItem(item);
    meshPlot_->repaint();
}

void AddItemDirector::createWidgets()
{
    QVBoxLayout *mainLayout = new QVBoxLayout;
    mainLayout->setMargin(0);
    mainLayout->setSpacing(0);
    setLayout(mainLayout);

    widgets_.append(new PointAddItemWidget(this));
    widgets_.append(new LineAddItemWidget(this));

    foreach (AddItemWidget *widget, widgets_)
        mainLayout->addWidget(widget);
}

void AddItemDirector::changeState(AddItemDirectorState *state)
{
    state_ = state;
}
