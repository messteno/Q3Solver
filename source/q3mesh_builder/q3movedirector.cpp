#include "q3plot.h"
#include "q3movedirector.h"

Q3MoveDirector::Q3MoveDirector(QWidget *parent) :
    Q3Director(Q3Director::Move, parent)
{

}

Q3MoveDirector::~Q3MoveDirector()
{

}

bool Q3MoveDirector::processDragged(Q3Plot *plot, Q3Sceleton *sceleton,
                                    const QPointF &oldScenePos,
                                    const QPointF &newScenePos,
                                    bool snapToGrid)
{
    QPointF diffScenePos = newScenePos - oldScenePos;
    plot->moveScene(diffScenePos);
    setActive(true);
    return true;
}

bool Q3MoveDirector::processDropped(Q3Plot *plot, Q3Sceleton *sceleton,
                                    const QPointF &scenePos, bool snapToGrid)
{
    setActive(false);
}

void Q3MoveDirector::stop()
{
    setActive(false);
}

void Q3MoveDirector::draw(Q3Painter &painter) const
{

}

