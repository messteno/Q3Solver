#include "q3circleform.h"
#include "q3pointconnectionform.h"
#include "q3pointform.h"
#include "q3sceleton.h"
#include "q3plot.h"
#include "q3selectdirector.h"
#include "ui_q3selectdirector.h"

Q3SelectDirector::Q3SelectDirector(QWidget *parent) :
    Q3Director(Q3Director::Select, parent),
    ui(new Ui::Q3SelectDirector),
    editForm_(NULL),
    editable_(false),
    movable_(false),
    moving_(false)
{
    ui->setupUi(this);
    this->hide();
}

Q3SelectDirector::~Q3SelectDirector()
{
    foreach (Q3SceletonItem *item, selectedItems_)
        item->setSelected(false);

    selectedItems_.clear();
}

bool Q3SelectDirector::processClick(QMouseEvent *event, const QPointF &scenePos)
{
    if (!plot_ || !sceleton_)
        return false;

    if (itemType_ != Q3SceletonItem::Base)
        return false;

    qreal radius = SelectRadius / plot_->sx();
    Q3SceletonItem *item = sceleton_->itemAt(scenePos, radius);

    if (event->button() == Qt::LeftButton &&
        event->modifiers() != Qt::ControlModifier)
    {
        foreach (Q3SceletonItem *deselectItem, selectedItems_)
            deselectItem->setSelected(false);
        selectedItems_.clear();
    }

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
    else if (event->modifiers() != Qt::ControlModifier)
    {
        foreach (Q3SceletonItem *deselectItem, selectedItems_)
            deselectItem->setSelected(false);
        selectedItems_.clear();
    }

    if (selectedItems_.empty())
        setActive(false);
    else
        setActive(true);

    if (editable_)
        setupEditForm();

    return false;
}

bool Q3SelectDirector::processDragged(const QPointF &oldScenePos,
                                      const QPointF &newScenePos)
{
    if (!movable_)
        return false;

    if (!plot_ || !sceleton_)
        return false;

    qreal radius = SelectRadius / plot_->sx();
    Q3SceletonItem *item = sceleton_->itemAt(oldScenePos, radius);
    if (!item && !moving_)
        return false;

    if (!moving_)
    {
        if (!selectedItems_.contains(item))
        {
            foreach (Q3SceletonItem *item, selectedItems_)
                item->setSelected(false);
            selectedItems_.clear();
        }

        if (selectedItems_.empty())
        {
            selectedItems_.append(item);
            item->setSelected(true);
        }
    }

    QPointF diffScenePos = newScenePos - oldScenePos;

    foreach (Q3SceletonItem *item, selectedItems_)
        item->move(diffScenePos);

    foreach (Q3SceletonItem *item, selectedItems_)
        item->setMoved(false);

    moving_ = true;
    setActive(true);

    setupEditForm();

    return true;
}

bool Q3SelectDirector::processDropped(const QPointF &scenePos)
{
    if (!movable_ || !moving_)
        return false;

    moving_ = false;

    if (!plot_)
        return false;

    if (selectedItems_.empty())
        return false;

    foreach (Q3SceletonItem *item, selectedItems_)
    {
        QPointF oldCenter = item->boundingRect().center();
        QPointF newCenter = plot_->snapScenePosToGrid(oldCenter);
        item->move(newCenter - oldCenter);

        if (itemType_ != Q3SceletonItem::Base)
            item->setSelected(false);
    }
    if (itemType_ != Q3SceletonItem::Base)
    {
        selectedItems_.clear();
        setActive(false);
    }

    emit itemMoved();
    setupEditForm();

    return true;
}

bool Q3SelectDirector::processKeyRelease(int key)
{
    if (!sceleton_)
        return false;

    if (!isActive())
        return false;

    switch (key)
    {
        case Qt::Key_Delete:
            sceleton_->removeSelectedItems();
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
    setupEditForm();
}

void Q3SelectDirector::draw(Q3Painter &painter) const
{

}

bool Q3SelectDirector::movable() const
{
    return movable_;
}

void Q3SelectDirector::setMovable(bool movable)
{
    movable_ = movable;
}

bool Q3SelectDirector::editable() const
{
    return editable_;
}

void Q3SelectDirector::setEditable(bool editable)
{
    editable_ = editable;
}

void Q3SelectDirector::setupEditForm()
{
    delete editForm_;
    editForm_ = NULL;

    if (itemType_ != Q3SceletonItem::Base || selectedItems_.empty() ||
        selectedItems_.count() != 1)
    {
        this->hide();
        return;
    }

    Q3SceletonItem *item = selectedItems_.first();
    switch (item->type())
    {
        case Q3SceletonItem::Point:
            editForm_ = new Q3PointForm(this);
            break;
        case Q3SceletonItem::PointConnection:
        {
            editForm_ = new Q3PointConnectionForm(this);
            foreach (Q3SceletonItem *checkedItem, sceleton_->items())
            {
                if (checkedItem->type() == Q3SceletonItem::Point)
                    checkedItem->accept(*editForm_);
            }
            break;
        }
        case Q3SceletonItem::Circle:
            editForm_ = new Q3CircleForm(this);
            break;
        default:
            break;
    }

    if (editForm_)
    {
        ui->elementFormLayout->addWidget(editForm_);
        item->accept(*editForm_);
        this->show();
    }
    else
        this->hide();
}

void Q3SelectDirector::on_editElementButton_clicked()
{
    if (editForm_ && selectedItems_.count() == 1)
    {
        Q3SceletonItem *item = selectedItems_.first();
        item->accept(*editForm_);
    }
    plot_->update();
    emit itemMoved();
}
