#include "additemdirector.h"
#include "additemdirectorstateitemadd.h"
#include "additemdirectorstateitemselect.h"

AddItemDirectorStateItemAdd::AddItemDirectorStateItemAdd()
{
}

AddItemDirectorState* AddItemDirectorStateItemAdd::getInstance()
{
    static AddItemDirectorStateItemAdd self;
    return &self;
}

void AddItemDirectorStateItemAdd::widgetButtonPushed(AddItemDirector *director, AddItemWidget *widget)
{
    QMeshItem *item = widget->getItem();
    if (item)
    {
        director->addItem(item);
        director->show();
        changeState(director, AddItemDirectorStateItemSelect::getInstance());
    }
}
