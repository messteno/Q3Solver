#include "additemdirector.h"
#include "additemdirectorstate.h"

AddItemDirectorState::AddItemDirectorState()
{
}

void AddItemDirectorState::widgetButtonPushed(AddItemDirector *director, AddItemWidget *widget)
{

}

void AddItemDirectorState::changeState(AddItemDirector *director, AddItemDirectorState *state)
{
    director->changeState(state);
}
