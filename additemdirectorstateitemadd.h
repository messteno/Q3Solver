#ifndef ADDITEMDIRECTORSTATEITEMADD_H
#define ADDITEMDIRECTORSTATEITEMADD_H

#include "additemdirectorstate.h"

class AddItemDirectorStateItemAdd : public AddItemDirectorState
{
public:
    static AddItemDirectorState* getInstance();
    virtual void widgetButtonPushed(AddItemDirector *director, AddItemWidget *widget);
private:
    AddItemDirectorStateItemAdd();
};

#endif // ADDITEMDIRECTORSTATEITEMADD_H
