#ifndef Q3ADDITEMDIRECTOR_H
#define Q3ADDITEMDIRECTOR_H

#include <QWidget>

#include "q3director.h"
#include "q3painter.h"
#include "q3sceletonitem.h"
#include "q3sceletonitemform.h"
#include "q3sceleton.h"

namespace Ui {
    class Q3AddItemDirector;
}

class Q3AddItemDirector : public Q3Director
{
    Q_OBJECT

public:
    Q3AddItemDirector(QWidget *parent = NULL);
    virtual ~Q3AddItemDirector();

    virtual void setItemType(Q3SceletonItem::Type type);
    virtual bool processClick(QMouseEvent *event, const QPointF &scenePos);
    virtual bool processDragged(const QPointF &oldScenePos,
                                const QPointF &newScenePos);
    virtual bool processMoved(const QPointF &oldScenePos,
                              const QPointF &newScenePos);
    virtual bool processKeyRelease(int key);
    virtual void stop();
    virtual void draw(Q3Painter &painter) const;

protected:
    virtual bool isActive();

public slots:
    void setupAddForm();

private slots:
    void on_createElementButton_clicked();

private:
    Q3SceletonItem *item_;
    Ui::Q3AddItemDirector *ui;
    Q3SceletonItemForm *addForm_;
};

#endif // Q3ADDITEMDIRECTOR_H
