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
    bool ok = widget->addItem();
    if (ok)
    {
        director->show();
        changeState(director, AddItemDirectorStateItemSelect::getInstance());
    }
}
