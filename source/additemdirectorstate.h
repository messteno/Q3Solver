#ifndef ADDITEMDIRECTORSTATE_H
#define ADDITEMDIRECTORSTATE_H

class AddItemDirector;
class AddItemWidget;

class AddItemDirectorState
{
public:
    virtual void widgetButtonPushed(AddItemDirector* director, AddItemWidget *widget);
protected:
    AddItemDirectorState();
    void changeState(AddItemDirector *director, AddItemDirectorState *state);
};

#endif // ADDITEMDIRECTORSTATE_H
