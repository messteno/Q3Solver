#include "pointadditemwidget.h"
#include "lineadditemwidget.h"
#include "additemdirector.h"
#include "additemdirectorstateitemselect.h"
#include "qmeshplot.h"

#include <QDebug>
#include <QStackedWidget>
#include <QVBoxLayout>

AddItemDirector::AddItemDirector(QMeshPlot *meshPlot, QWidget *parent) :
    QWidget(parent)
{
    meshPlot_ = meshPlot;
    state_ = AddItemDirectorStateItemSelect::getInstance();

    mainLayout_ = new QStackedLayout;
    mainLayout_->setMargin(0);
    mainLayout_->setSpacing(0);
    setLayout(mainLayout_);
    connect(mainLayout_, SIGNAL(currentChanged(int)), this, SLOT(layoutWidgetChanged(int)));

    createWidgets();

    hide();
}

AddItemDirector::~AddItemDirector()
{
    foreach (AddItemWidget *widget, widgets_)
        delete widget;
}

void AddItemDirector::widgetButtonPushed(AddItemWidget *selectedWidget)
{
    state_->widgetButtonPushed(this, selectedWidget);
}

void AddItemDirector::processWidgetSelected (AddItemWidget *selectedWidget)
{
    mainLayout_->setCurrentWidget(selectedWidget);
}

void AddItemDirector::show()
{
    state_ = AddItemDirectorStateItemSelect::getInstance();
    mainLayout_->setCurrentIndex(0);
    QWidget::show();
}

void AddItemDirector::hide()
{
    state_ = AddItemDirectorStateItemSelect::getInstance();
    mainLayout_->setCurrentIndex(0);
    QWidget::hide();
}

void AddItemDirector::addItem(QMeshItem *item)
{
    meshPlot_->addItem(item);
    meshPlot_->repaint();
}

void AddItemDirector::createWidgets()
{
    widgets_.append(new PointAddItemWidget(this, trUtf8("Добавить точку")));
    widgets_.append(new LineAddItemWidget(this, trUtf8("Добавить отрезок")));

    QWidget *addButtonsWidget = new QWidget(this);
    QVBoxLayout *addButtonsWidgetLayout = new QVBoxLayout(addButtonsWidget);
    addButtonsWidget->setLayout(addButtonsWidgetLayout);

    foreach (AddItemWidget *widget, widgets_)
    {
        QPushButton *widgetButton = widget->getAddButton();
        connect (widgetButton, SIGNAL(clicked()), widget, SLOT(selected()));
        addButtonsWidgetLayout->addWidget(widgetButton);
    }

    mainLayout_->addWidget(addButtonsWidget);
    foreach (AddItemWidget *widget, widgets_)
    {
        mainLayout_->addWidget(widget);
        widget->setSizePolicy(QSizePolicy::Ignored, QSizePolicy::Ignored);
    }
}

void AddItemDirector::layoutWidgetChanged(int index)
{
    QWidget *newWidget = mainLayout_->itemAt(index)->widget();
    if (!newWidget)
        return;
    for (int i = 0; i < mainLayout_->count(); ++i)
    {
        QWidget *widget = mainLayout_->itemAt(i)->widget();
        if (widget)
            widget->setSizePolicy(QSizePolicy::Ignored, QSizePolicy::Ignored);
    }
    newWidget->setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Minimum);
}

void AddItemDirector::changeState(AddItemDirectorState *state)
{
    state_ = state;
}
