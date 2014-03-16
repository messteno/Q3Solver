#ifndef ADDITEMDIRECTOR_H
#define ADDITEMDIRECTOR_H

#include <QList>
#include <QPushButton>
#include <QStackedLayout>
#include "additemwidget.h"
#include "additemdirectorstate.h"

class QMesh;
class QMeshPlot;
class QMeshItem;

class AddItemDirector : public QWidget
{
    Q_OBJECT
public:
    AddItemDirector(QMesh *mesh);
    virtual ~AddItemDirector();
    virtual void widgetButtonPushed(AddItemWidget *widget);
    virtual void processWidgetSelected (AddItemWidget *selectedWidget);
    virtual void show();
    virtual void hide();
    virtual void addItem(QMeshItem* item);

protected:
    virtual void createWidgets();

public slots:
    void layoutWidgetChanged(int index);
    void meshPlotClicked(QMeshPlot *meshPlot);

private:
    friend class AddItemDirectorState;
    void changeState (AddItemDirectorState *state);

private:
    QMesh *mesh_;
    QStackedLayout *mainLayout_;
    AddItemDirectorState *state_;
    QList<AddItemWidget *> widgets_;
    AddItemWidget *currentWidget_;
};

#endif // ADDITEMDIRECTOR_H
