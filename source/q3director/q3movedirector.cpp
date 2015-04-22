#include "q3plot.h"
#include "q3movedirector.h"

Q3MoveDirector::Q3MoveDirector(QWidget *parent) :
    Q3Director(Q3Director::Move, parent)
{

}

Q3MoveDirector::~Q3MoveDirector()
{

}

bool Q3MoveDirector::processDragged(const QPointF &oldScenePos,
                                    const QPointF &newScenePos)
{
    if (!plot_)
        return false;

    QPointF diffScenePos = newScenePos - oldScenePos;
    plot_->moveScene(diffScenePos);
    setActive(true);
    return true;
}

bool Q3MoveDirector::processDropped(const QPointF &scenePos)
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

