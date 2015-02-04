#ifndef Q3SELECTDIRECTOR_H
#define Q3SELECTDIRECTOR_H

#include <QWidget>

#include "q3director.h"
#include "q3sceletonitem.h"

class Q3SelectDirector : public Q3Director
{
private:
    QList<Q3SceletonItem*> selectedItems_;
public:
    Q3SelectDirector(QWidget *parent = NULL);
    virtual ~Q3SelectDirector();

    virtual bool processClick(Q3Plot *plot,
                              Q3Sceleton *sceleton,
                              const QPointF &scenePos,
                              bool snapToGrid);
    virtual bool processDragged(Q3Plot *plot,
                                Q3Sceleton *sceleton,
                                const QPointF &oldScenePos,
                                const QPointF &newScenePos,
                                bool snapToGrid);
    virtual bool processDropped(Q3Plot *plot,
                                Q3Sceleton *sceleton,
                                const QPointF &scenePos,
                                bool snapToGrid);
    virtual bool processKeyRelease(Q3Plot *plot,
                                   Q3Sceleton *sceleton,
                                   int key, bool snapToGrid);
    virtual void stop();
    virtual void draw(Q3Painter &painter) const;
};

#endif // Q3SELECTEDDIRECTOR_H
