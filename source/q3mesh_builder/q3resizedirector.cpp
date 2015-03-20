#include "q3plot.h"
#include "q3sceleton.h"
#include "q3resizedirector.h"

Q3ResizeDirector::Q3ResizeDirector(QWidget *parent) :
    Q3Director(Q3Director::Resize, parent)
{

}

Q3ResizeDirector::~Q3ResizeDirector()
{
    resizingItems_.clear();
}

bool Q3ResizeDirector::processDragged(const QPointF &oldScenePos,
                                      const QPointF &newScenePos,
                                      bool snapToGrid)
{
    if (!plot_ || !sceleton_)
        return false;

    if (resizingItems_.empty())
    {
        qreal radius = SelectRadius / plot_->sx();
        Q3SceletonItem *item = sceleton_->itemToResizeAt(oldScenePos, radius);
        if (item)
        {
            resizingItems_.append(item);
            item->setResizing(true);
        }
        else
            return false;
    }

    foreach (Q3SceletonItem *item, resizingItems_)
        item->resize(oldScenePos, newScenePos);

    setActive(true);
    return true;
}

bool Q3ResizeDirector::processDropped(const QPointF &scenePos, bool snapToGrid)
{
    if (!plot_)
        return false;

    if (resizingItems_.empty())
        return false;

    foreach (Q3SceletonItem *item, resizingItems_)
    {
        if (snapToGrid)
        {
            // TODO: maybe set only one coordinate tot resize?
            QPointF oldCenter = item->boundingRect().center();
            QPointF snappedScenePos = plot_->snapScenePosToGrid(scenePos);
            item->resize(oldCenter, snappedScenePos);
        }
        if (itemType_ != Q3SceletonItem::Base)
            item->setResizing(false);
    }
    resizingItems_.clear();
    setActive(false);

    emit itemResized();

    return true;
}

void Q3ResizeDirector::stop()
{
    foreach (Q3SceletonItem *item, resizingItems_)
        item->setResizing(false);
    resizingItems_.clear();
    setActive(false);
}

void Q3ResizeDirector::draw(Q3Painter &painter) const
{

}
