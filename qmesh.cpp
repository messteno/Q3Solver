#include <QDebug>
#include <QPainter>
#include "qmesh.h"

QMesh::QMesh(QWidget *parent) :
    QWidget(parent)
{
   sceneRect_ = QRectF(-2, -4, 10, 10);
   drawRect_ = sceneRect_;
   scaleX_ = 1.;
   scaleY_ = -1.;

   addItem(new QMeshRectItem(QRectF(-1, 1, 3, 3)));
}

QMesh::~QMesh()
{
    foreach (QMeshItem *item, items_)
        delete item;
    items_.clear();
}

void QMesh::resizeEvent(QResizeEvent *event)
{
    updateScene();
}

void QMesh::paintEvent(QPaintEvent *event)
{
    drawAxes();
    drawItems();
}

void QMesh::addItem(QMeshItem *item)
{
    items_.push_back(item);
}

void QMesh::updateScene()
{
    qreal dw = sceneRect_.width();
    qreal dh = sceneRect_.height();
    if (dw / dh > width() / height())
        dh = dw / width() * height();
    else
        dw = dh / height() * width();
    drawRect_.setTopLeft(sceneRect_.center() - QPointF(0.5 * dw, 0.5 * dh));
    drawRect_.setBottomRight(sceneRect_.center() + QPointF(0.5 * dw, 0.5 * dh));

    scaleX_ = width() / dw;
    scaleY_ = - height() / dh;

    dx_ = sceneToMapX (0);
    dy_ = sceneToMapY (0);
}

qreal QMesh::sceneToMapX (qreal x)
{
    return width() * (x - drawRect_.x()) / drawRect_.width();
}

qreal QMesh::sceneToMapY (qreal y)
{
    return height() * (1. - (y - drawRect_.y()) / drawRect_.height());
}

void QMesh::drawAxes()
{
    QPainter painter;
    painter.begin(this);
    painter.drawLine(0, sceneToMapY(0), width(), sceneToMapY(0));
    painter.drawLine(sceneToMapX(0), 0, sceneToMapX(0), height());
    painter.end();
}

void QMesh::drawItems()
{
    QPainter painter;
    painter.begin(this);
    painter.save();
    painter.translate(dx_, dy_);
    foreach (QMeshItem *item, items_)
        item->draw(painter, scaleX_, scaleY_);
    painter.restore();
    painter.end();
}
