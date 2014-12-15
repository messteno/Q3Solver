#include <math.h>

#include <QDebug>

#include "q3pointconnection.h"
#include "q3point.h"
#include "q3meshbuilder.h"
#include "ui_q3meshbuilder.h"

const int Q3MeshBuilder::SelectRadius = 10;

Q3MeshBuilder::Q3MeshBuilder(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::Q3MeshBuilder),
    processMode_(NoProcess),
    sceleton_(NULL),
    addItem_(NULL),
    itemType_(Q3SceletonItem::Base)
{
    ui->setupUi(this);
    sceleton_ = new Q3Sceleton(this);
    ui->plotWidget->setSceleton(sceleton_);

    connect(ui->plotWidget, SIGNAL(mouseClicked(const QPointF)),
            this, SLOT(plotMouseClicked(const QPointF)));

    connect(ui->plotWidget, SIGNAL(mouseDragged(const QPointF, const QPointF)),
            this, SLOT(plotMouseDragged(const QPointF, const QPointF)));

    connect(ui->plotWidget, SIGNAL(mouseDropped(const QPointF)),
            this, SLOT(plotMouseDropped(const QPointF)));
}

Q3MeshBuilder::~Q3MeshBuilder()
{
    delete ui;

    // Does it really needed?
    delete sceleton_;
    sceleton_ = NULL;
}

Q3MeshBuilder::ProcessMode Q3MeshBuilder::processMode() const
{
    return processMode_;
}

void Q3MeshBuilder::setProcessMode(Q3MeshBuilder::ProcessMode mode)
{
    if (mode != AddItemProcess && addItem_)
    {
        delete addItem_;
        addItem_ = NULL;
    }
    processMode_ = mode;
}

Q3SceletonItem::Type Q3MeshBuilder::itemType() const
{
    return itemType_;
}

void Q3MeshBuilder::setItemType(Q3SceletonItem::Type type)
{
    itemType_ = type;
}

void Q3MeshBuilder::plotMouseClicked(const QPointF scenePos)
{
    QPointF pos = scenePos;
    if (ui->snapToGrid->isChecked())
        pos = ui->plotWidget->snapScenePosToGrid(pos);

    if (itemType_ == Q3SceletonItem::Point)
    {
        Q3Point *point = new Q3Point(pos);
        sceleton_->addItem(point);
    }
    else if (itemType_ == Q3SceletonItem::PointConnection)
    {
        if (processMode_ == NoProcess)
        {
            qreal radius = SelectRadius / ui->plotWidget->sx();
            Q3SceletonItem *item = sceleton_->itemAt(scenePos, radius,
                                                     Q3SceletonItem::Point);
            if (item)
            {
                Q3Point *a = dynamic_cast<Q3Point *>(item);
                if (a)
                {
                    addItem_ = new Q3PointConnection(a, a);
                    setProcessMode(AddItemProcess);
                }
            }
        }
        else if (processMode_ == AddItemProcess)
        {
            qreal radius = SelectRadius / ui->plotWidget->sx();
            Q3SceletonItem *item = sceleton_->itemAt(scenePos, radius,
                                                     Q3SceletonItem::Point);
            if (item)
            {
                Q3PointConnection *conn = dynamic_cast<Q3PointConnection *>(addItem_);
                Q3Point *b = dynamic_cast<Q3Point *>(item);
                if (b && conn && conn->a() != b)
                {
                    addItem_ = NULL;
                    conn->setB(b);
                    sceleton_->addItem(conn);
                    setProcessMode(NoProcess);
                }
            }
        }
    }
    ui->plotWidget->update();
}

void Q3MeshBuilder::plotMouseDragged(const QPointF oldScenePos,
                                     const QPointF newScenePos)
{
    if (processMode_ == NoProcess)
    {
        qreal radius = SelectRadius / ui->plotWidget->sx();
        Q3SceletonItem *item = sceleton_->itemAt(oldScenePos, radius);
        if (item)
        {
            selectedItems_.append(item);
            item->setSelected(true);
            setProcessMode(MoveItemProcess);
        }
        else
            setProcessMode(MoveProcess);
    }

    QPointF diffScenePos = newScenePos - oldScenePos;
    if (processMode_ == MoveProcess || processMode_ == AddItemProcess)
    {
        ui->plotWidget->moveScene(diffScenePos);
    }
    else if (processMode_ == MoveItemProcess)
    {
        foreach (Q3SceletonItem *item, selectedItems_)
            item->move(diffScenePos);
    }
    ui->plotWidget->update();
}

void Q3MeshBuilder::plotMouseDropped(const QPointF scenePos)
{
    if (processMode_ == MoveProcess)
    {
        setProcessMode(NoProcess);
    }
    else if (processMode_ == MoveItemProcess)
    {
        foreach (Q3SceletonItem *item, selectedItems_)
        {
            if (ui->snapToGrid->isChecked())
            {
                QPointF oldCenter = item->boundingRect().center();
                QPointF newCenter = ui->plotWidget->snapScenePosToGrid(oldCenter);
                item->move(newCenter - oldCenter);
            }
            item->setSelected(false);
        }
        selectedItems_.clear();
        setProcessMode(NoProcess);
    }
    ui->plotWidget->update();
}

void Q3MeshBuilder::on_pointButton_clicked(bool checked)
{
    if (checked)
    {
        ui->pointConnectionButton->setChecked(false);
        setItemType(Q3SceletonItem::Point);
    }
    else
        setItemType(Q3SceletonItem::Base);
    setProcessMode(NoProcess);
}

void Q3MeshBuilder::on_pointConnectionButton_clicked(bool checked)
{
    if (checked)
    {
        ui->pointButton->setChecked(false);
        setItemType(Q3SceletonItem::PointConnection);
    }
    else
        setItemType(Q3SceletonItem::Base);
    setProcessMode(NoProcess);
}
