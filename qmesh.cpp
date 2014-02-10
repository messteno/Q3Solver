#include <QDebug>
#include <QPainter>
#include <QWheelEvent>
#include <qmath.h>
#include "qmesh.h"

QMesh::QMesh(QWidget *parent) :
    QWidget(parent)
{
   sceneRect_ = QRectF(-2, -4, 10, 10);
   drawRect_ = sceneRect_;
   scaleX_ = 1.;
   scaleY_ = -1.;
   setMouseTracking(true);

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

void QMesh::wheelEvent(QWheelEvent *event)
{
    int numSteps = event->delta() / 15 / 8;
    if (numSteps == 0)
    {
        event->ignore();
        return;
    }
    qreal sc = pow(0.9, numSteps);
    QPointF scenePos = mapToScene(event->pos());
    qreal sxl = scenePos.x() - drawRect_.x();
    qreal syl = scenePos.y() - drawRect_.y();
    qreal sxr = drawRect_.x() + drawRect_.width() - scenePos.x();
    qreal syr = drawRect_.y() + drawRect_.height() - scenePos.y();
    sceneRect_.setTopLeft(QPointF(scenePos.x() - sxl * sc, scenePos.y() - syl * sc));
    sceneRect_.setBottomRight(QPointF(scenePos.x() + sxr * sc, scenePos.y() + syr * sc));
    updateScene();
    repaint();
}

void QMesh::mouseReleaseEvent(QMouseEvent *)
{
    mousePos_ = QPointF();
}

void QMesh::mouseMoveEvent(QMouseEvent *event)
{
    if (event->buttons() & Qt::LeftButton)
    {
        if (mousePos_.isNull())
        {
            mousePos_ = event->pos();
        }
        else
        {
            QPointF oldScenePos = mapToScene(mousePos_);
            QPointF newScenePos = mapToScene(event->pos());
            QPointF diffPos = newScenePos - oldScenePos;
            sceneRect_ = QRectF(sceneRect_.x() - diffPos.x(),
                                sceneRect_.y() - diffPos.y(),
                                sceneRect_.width(),
                                sceneRect_.height());
            mousePos_ = event->pos();
            updateScene();
            repaint();
        }
    }
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

QPointF QMesh::sceneToMap(QPointF pos)
{
    return QPointF(sceneToMapX(pos.x()), sceneToMapY(pos.y()));
}

QPointF QMesh::sceneToMap(qreal x, qreal y)
{
    return QPointF(sceneToMapX(x), sceneToMapY(y));
}

qreal QMesh::sceneToMapX (qreal x)
{
    return width() * (x - drawRect_.x()) / drawRect_.width();
}

qreal QMesh::sceneToMapY (qreal y)
{
    return height() * (1. - (y - drawRect_.y()) / drawRect_.height());
}

QPointF QMesh::mapToScene(QPointF pos)
{
    return QPointF(mapToSceneX(pos.x()), mapToSceneY(pos.y()));
}

QPointF QMesh::mapToScene(qreal x, qreal y)
{
    return QPointF(mapToSceneX(x), mapToSceneY(y));
}

qreal QMesh::mapToSceneX (qreal x)
{
    return drawRect_.x() + drawRect_.width() * x / width();
}

qreal QMesh::mapToSceneY (qreal y)
{
    return drawRect_.y() + drawRect_.height() * (height() - y) / height();
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
