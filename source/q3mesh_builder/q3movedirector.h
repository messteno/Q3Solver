#ifndef Q3MOVEDIRECTOR_H
#define Q3MOVEDIRECTOR_H

#include "q3director.h"

class Q3MoveDirector : public Q3Director
{
public:
    Q3MoveDirector(QWidget *parent = NULL);
    virtual ~Q3MoveDirector();
    virtual bool processDragged(const QPointF &oldScenePos,
                                const QPointF &newScenePos,
                                bool snapToGrid);
    virtual bool processDropped(const QPointF &scenePos,
                                bool snapToGrid);
    virtual void stop();
    virtual void draw(Q3Painter &painter) const;
};

#endif // Q3MOVEDIRECTOR_H
