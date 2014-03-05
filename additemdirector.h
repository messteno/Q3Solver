#ifndef ADDITEMDIRECTOR_H
#define ADDITEMDIRECTOR_H

#include <QList>
#include "additemwidget.h"
#include "additemdirectorstate.h"

class QMeshPlot;
class QMeshItem;

class AddItemDirector : public QWidget
{
public:
    AddItemDirector(QMeshPlot *meshPlot, QWidget *parent = 0);
    virtual ~AddItemDirector();
    virtual void widgetButtonPushed(AddItemWidget *widget);
    virtual void processWidgetSelected (AddItemWidget *selectedWidget);
    virtual void show();
    virtual void hide();
    virtual void addItem(QMeshItem* item);

protected:
    virtual void createWidgets();

private:
    friend class AddItemDirectorState;
    void changeState (AddItemDirectorState *state);
private:
    AddItemDirectorState *state_;
    QMeshPlot *meshPlot_;
    QList<AddItemWidget *> widgets_;
};

#endif // ADDITEMDIRECTOR_H
