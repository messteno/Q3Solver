#ifndef ADDITEMDIRECTORSTATEITEMSELECT_H
#define ADDITEMDIRECTORSTATEITEMSELECT_H

#include "additemdirectorstate.h"

class AddItemDirectorStateItemSelect : public AddItemDirectorState
{
public:
    static AddItemDirectorState* getInstance();
    virtual void widgetButtonPushed(AddItemDirector *director, AddItemWidget *widget);
private:
    AddItemDirectorStateItemSelect();
};

#endif // ADDITEMDIRECTORSTATEITEMSELECT_H
