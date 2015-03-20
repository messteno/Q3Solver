#include <qmath.h>
#include <QVector2D>

#include "q3pointconnectionform.h"
#include "q3circleform.h"
#include "q3pointform.h"
#include "q3pointconnection.h"
#include "q3plot.h"
#include "q3point.h"
#include "q3circle.h"
#include "q3sceleton.h"
#include "q3additemdirector.h"
#include "ui_q3additemdirector.h"

#include <QDebug>

Q3AddItemDirector::Q3AddItemDirector(QWidget *parent) :
    Q3Director(Q3Director::Add, parent),
    item_(NULL),
    addForm_(NULL),
    ui(new Ui::Q3AddItemDirector)
{
    ui->setupUi(this);
    this->hide();
}

Q3AddItemDirector::~Q3AddItemDirector()
{
    stop();
}

void Q3AddItemDirector::setItemType(Q3SceletonItem::Type type)
{
    if (itemType_ != type)
        stop();
    itemType_ = type;

    setupAddForm();
}

void Q3AddItemDirector::stop()
{
    if (item_)
    {
        switch (item_->type())
        {
            case Q3SceletonItem::PointConnection:
            {
                Q3PointConnection *conn =
                        dynamic_cast<Q3PointConnection *>(item_);
                if (conn)
                    delete conn->b();
                break;
            }
            default:
                break;
        }
        delete item_;
    }
    item_ = NULL;
}

bool Q3AddItemDirector::isActive()
{
    return item_ != NULL;
}

void Q3AddItemDirector::draw(Q3Painter &painter) const
{
    if (item_)
        item_->draw(painter);
}

bool Q3AddItemDirector::processClick(QMouseEvent *event, const QPointF &scenePos,
                                     bool snapToGrid)
{
    if (!plot_ || !sceleton_)
        return false;

    QPointF snappedScenePos = scenePos;
    if (snapToGrid)
        snappedScenePos = plot_->snapScenePosToGrid(scenePos);

    qreal radius = SelectRadius / plot_->sx();

    if (isActive())
    {
        switch (itemType_)
        {
            case Q3SceletonItem::PointConnection:
            {
                Q3PointConnection *conn =
                        dynamic_cast<Q3PointConnection *>(item_);
                Q3Point *b = dynamic_cast<Q3Point *>
                        (sceleton_->itemAt(scenePos, radius,
                                          Q3SceletonItem::Point));
                if (b && conn && conn->a() != b)
                {
                    Q3Point *oldB = conn->b();
                    conn->setB(b);
                    delete oldB;
                    item_ = NULL;
                    sceleton_->addItem(conn);
                    return true;
                }
                break;
            }
            case Q3SceletonItem::Circle:
            {
                Q3Circle *circle = dynamic_cast<Q3Circle *>(item_);
                if (circle)
                {
                    QVector2D radiusVector(snappedScenePos - circle->center());
                    if (snappedScenePos == circle->center())
                        radiusVector = QVector2D(scenePos - circle->center());
                    radius = radiusVector.length();
                    item_ = NULL;
                    circle->setRadius(radius);
                    sceleton_->addItem(circle);
                    return true;
                }
                break;
            }
            default:
                break;
        }
    }
    else
    {
        switch (itemType_)
        {
            case Q3SceletonItem::Point:
            {
                Q3SceletonItem *item = sceleton_->itemAt(scenePos, radius,
                                                        Q3SceletonItem::Point);
                if (!item)
                {
                    Q3Point *point = new Q3Point(snappedScenePos);
                    sceleton_->addItem(point);
                    return true;
                }
                break;
            }
            case Q3SceletonItem::PointConnection:
            {
                // TODO: Не создавать отрезок, если две точки уже соединены
                Q3Point *a = dynamic_cast<Q3Point *>
                        (sceleton_->itemAt(scenePos, radius,
                                          Q3SceletonItem::Point));
                if (a)
                {
                    Q3Point *b = new Q3Point(*a);
                    item_ = new Q3PointConnection(a, b);
                    return true;
                }
                break;
            }
            case Q3SceletonItem::Circle:
            {
                Q3Circle *circle = new Q3Circle(snappedScenePos, 0);
                item_ = circle;
                return true;
            }
            default:
                break;
        }
    }
    return false;
}

bool Q3AddItemDirector::processDragged(const QPointF &oldScenePos,
                                       const QPointF &newScenePos,
                                       bool snapToGrid)
{
    if (!plot_ || !sceleton_)
        return false;

    if (isActive())
    {
        QPointF diffScenePos = newScenePos - oldScenePos;
        plot_->moveScene(diffScenePos);
        return true;
    }
    return false;
}

bool Q3AddItemDirector::processMoved(const QPointF &oldScenePos,
                                     const QPointF &newScenePos,
                                     bool snapToGrid)
{
    if (!isActive())
        return false;

    switch (itemType_)
    {
        case Q3SceletonItem::PointConnection:
        {
            Q3PointConnection *conn = dynamic_cast<Q3PointConnection *>(item_);
            if (conn)
            {
                Q3Point *b = conn->b();
                QPointF diffScenePos = newScenePos - oldScenePos;
                b->move(diffScenePos);
            }
            break;
        }
        case Q3SceletonItem::Circle:
        {
            Q3Circle *circle = dynamic_cast<Q3Circle *>(item_);
            if (circle)
            {
                QPointF radiusVector = newScenePos - circle->center();
                qreal radius = sqrt(radiusVector.x() * radiusVector.x() +
                                    radiusVector.y() * radiusVector.y());
                circle->setRadius(radius);
            }
            return true;
        }
        default:
            break;
    }
    return false;
}

bool Q3AddItemDirector::processKeyRelease(int key, bool snapToGrid)
{
    if (!isActive())
        return false;

    switch (key)
    {
        case Qt::Key_Escape:
            stop();
            return true;
        default:
            return false;
    }
    return false;
}

void Q3AddItemDirector::on_createElementButton_clicked()
{
    if (addForm_)
    {
        Q3SceletonItem *item = addForm_->createItem();
        if (item)
        {
            sceleton_->addItem(item);
            addForm_->clear();
            plot_->update();
        }
    }
}

void Q3AddItemDirector::setupAddForm()
{
    delete addForm_;
    addForm_ = NULL;

    switch(itemType_)
    {
        case Q3SceletonItem::Base:
            this->hide();
            break;
        case Q3SceletonItem::Point:
            addForm_ = new Q3PointForm(this);
            ui->elementFormLayout->addWidget(addForm_);
            this->show();
            break;
        case Q3SceletonItem::PointConnection:
        {
            addForm_ = new Q3PointConnectionForm(this);
            ui->elementFormLayout->addWidget(addForm_);
            foreach (Q3SceletonItem *item, sceleton_->items())
            {
                if(item->type() == Q3SceletonItem::Point)
                    item->accept(*addForm_);
            }
            this->show();
            break;
        }
        case Q3SceletonItem::Circle:
            addForm_ = new Q3CircleForm(this);
            ui->elementFormLayout->addWidget(addForm_);
            this->show();
            break;
        default:
            this->show();
            break;
    }
}
