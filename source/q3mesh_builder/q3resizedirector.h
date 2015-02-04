#ifndef Q3RESIZEDIRECTOR_H
#define Q3RESIZEDIRECTOR_H

#include <QWidget>

#include "q3director.h"
#include "q3sceletonitem.h"

class Q3ResizeDirector : public Q3Director
{
private:
    QList<Q3SceletonItem*> resizingItems_;
public:
    Q3ResizeDirector(QWidget *parent = NULL);
    virtual ~Q3ResizeDirector();

    virtual bool processDragged(Q3Plot *plot,
                                Q3Sceleton *sceleton,
                                const QPointF &oldScenePos,
                                const QPointF &newScenePos,
                                bool snapToGrid);
    virtual bool processDropped(Q3Plot *plot,
                                Q3Sceleton *sceleton,
                                const QPointF &scenePos,
                                bool snapToGrid);
    virtual void stop();
    virtual void draw(Q3Painter &painter) const;
};

#endif // Q3RESIZEDIRECTOR_H
