#ifndef Q3ADDITEMDIRECTOR_H
#define Q3ADDITEMDIRECTOR_H

#include <QWidget>

#include "q3director.h"
#include "q3painter.h"
#include "q3sceletonitem.h"
#include "q3sceleton.h"

namespace Ui {
    class Q3AddItemDirector;
}

class Q3AddItemDirector : public Q3Director
{
    Q_OBJECT

private:
    Q3SceletonItem *item_;
    Ui::Q3AddItemDirector *ui;

protected:
    virtual bool isActive();

public:
    Q3AddItemDirector(QWidget *parent = NULL);
    virtual ~Q3AddItemDirector();

    virtual void setItemType(Q3SceletonItem::Type type);
    virtual bool processClick(Q3Plot *plot,
                              Q3Sceleton *sceleton,
                              const QPointF &scenePos,
                              bool snapToGrid);
    virtual bool processDragged(Q3Plot *plot,
                                Q3Sceleton *sceleton,
                                const QPointF &oldScenePos,
                                const QPointF &newScenePos,
                                bool snapToGrid);
    virtual bool processMoved(Q3Plot *plot,
                              Q3Sceleton *sceleton,
                              const QPointF &oldScenePos,
                              const QPointF &newScenePos,
                              bool snapToGrid);
    virtual bool processKeyRelease(Q3Plot *plot,
                                   Q3Sceleton *sceleton,
                                   int key, bool snapToGrid);
    virtual void stop();
    virtual void draw(Q3Painter &painter) const;
};

#endif // Q3ADDITEMDIRECTOR_H
