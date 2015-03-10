#include "q3plot.h"
#include "q3sceleton.h"
#include "q3selectdirector.h"

Q3SelectDirector::Q3SelectDirector(QWidget *parent) :
    Q3Director(parent)
{

}

Q3SelectDirector::~Q3SelectDirector()
{
    selectedItems_.clear();
}

bool Q3SelectDirector::processClick(Q3Plot *plot, Q3Sceleton *sceleton,
                                    const QPointF &scenePos, bool snapToGrid)
{
    if (itemType_ != Q3SceletonItem::Base)
        return false;

    qreal radius = SelectRadius / plot->sx();
    Q3SceletonItem *item = sceleton->itemAt(scenePos, radius);
    if (item)
    {
        if (!selectedItems_.contains(item))
        {
            selectedItems_.append(item);
            item->setSelected(true);
        }
        else
        {
            item->setSelected(false);
            selectedItems_.removeAll(item);
        }
    }
    else
    {
        foreach (Q3SceletonItem *item, selectedItems_)
            item->setSelected(false);
        selectedItems_.clear();
    }

    if (selectedItems_.empty())
        setActive(false);
    else
        setActive(true);

    return false;
}

bool Q3SelectDirector::processDragged(Q3Plot *plot, Q3Sceleton *sceleton,
                                      const QPointF &oldScenePos,
                                      const QPointF &newScenePos,
                                      bool snapToGrid)
{
    if (selectedItems_.empty())
    {
        qreal radius = SelectRadius / plot->sx();
        Q3SceletonItem *item = sceleton->itemAt(oldScenePos, radius);
        if (item)
        {
            selectedItems_.append(item);
            item->setSelected(true);
        }
        else
            return false;
    }

    QPointF diffScenePos = newScenePos - oldScenePos;
    foreach (Q3SceletonItem *item, selectedItems_)
        item->move(diffScenePos);

    setActive(true);
    return true;
}

bool Q3SelectDirector::processDropped(Q3Plot *plot, Q3Sceleton *sceleton,
                                      const QPointF &scenePos, bool snapToGrid)
{
    if (selectedItems_.empty())
        return false;

    foreach (Q3SceletonItem *item, selectedItems_)
    {
        if (snapToGrid)
        {
            QPointF oldCenter = item->boundingRect().center();
            QPointF newCenter = plot->snapScenePosToGrid(oldCenter);
            item->move(newCenter - oldCenter);
        }
        if (itemType_ != Q3SceletonItem::Base)
            item->setSelected(false);
    }
    if (itemType_ != Q3SceletonItem::Base)
    {
        selectedItems_.clear();
        setActive(false);
    }

    return true;
}

bool Q3SelectDirector::processKeyRelease(Q3Plot *plot, Q3Sceleton *sceleton,
                                         int key, bool snapToGrid)
{
    if (!isActive())
        return false;

    switch (key)
    {
        case Qt::Key_Delete:
            sceleton->removeSelectedItems();
            selectedItems_.clear();
            stop();
            return true;
        default:
            return false;
    }
    return false;
}

void Q3SelectDirector::stop()
{
    foreach (Q3SceletonItem *item, selectedItems_)
        item->setSelected(false);
    selectedItems_.clear();
    setActive(false);
}

void Q3SelectDirector::draw(Q3Painter &painter) const
{

}
