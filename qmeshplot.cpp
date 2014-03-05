#include <QDebug>
#include <QPainter>
#include <QWheelEvent>
#include <QFont>
#include <string>
#include <iostream>
#include <sstream>
#include <qmath.h>

#include "qmeshplot.h"

QMeshPlot::QMeshPlot(QWidget *parent) :
    QWidget(parent)
{
   sceneRect_ = QRectF(-2, -4, 10, 10);
   drawRect_ = sceneRect_;
   scaleX_ = 1.;
   scaleY_ = -1.;
   tickDx_ = 1;
   tickDy_ = 1;
   countTickX_ = 10;
   countTickY_ = 10;

   setMouseTracking(true);
   setBackgroundColor(QColor(0x00, 0x16, 0x1c));
   setForegroundColor(QColor(0x1d, 0xd3, 0xf3, 0xa0));
   setAxesColor(QColor(0xff, 0xff, 0xff));
   setBottomMargin(25);
   setLeftMargin(20);
}

QMeshPlot::~QMeshPlot()
{
    foreach (QMeshItem *item, items_)
        delete item;
    items_.clear();
}

void QMeshPlot::resizeEvent(QResizeEvent *event)
{
    updateScene();
}

void QMeshPlot::paintEvent(QPaintEvent *event)
{
    drawBackground();
    drawAxes();
    drawItems();
    drawBorders();
}

void QMeshPlot::wheelEvent(QWheelEvent *event)
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

void QMeshPlot::mouseReleaseEvent(QMouseEvent *)
{
    mousePos_ = QPointF();
}

void QMeshPlot::mouseMoveEvent(QMouseEvent *event)
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

void QMeshPlot::addItem(QMeshItem *item)
{
    if (item)
        items_.push_back(item);
}

void QMeshPlot::updateScene()
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

    tickDx_ = pow (10, (int) (log(drawRect_.width()) / log(10.)));
    tickDy_ = pow (10, (int) (log(drawRect_.height()) / log(10.)));

    countTickX_ = ceil(drawRect_.width() / tickDx_);
    countTickY_ = ceil(drawRect_.height() / tickDy_);

    while (countTickX_ < 6)
    {
        tickDx_ /= 2.;
        countTickX_ = ceil(drawRect_.width() / tickDx_);
    }

    while (countTickY_ < 6)
    {
        tickDy_ /= 2.;
        countTickY_ = ceil(drawRect_.height() / tickDy_);
    }

    if ((tickDx_ / tickDy_ < 5) && (tickDy_ / tickDx_ < 5))
        tickDx_ = tickDy_ = qMin(tickDx_, tickDy_);

    countTickX_ = ceil(drawRect_.width() / tickDx_);
    countTickY_ = ceil(drawRect_.height() / tickDy_);
}

QPointF QMeshPlot::sceneToMap(const QPointF &pos) const
{
    return QPointF(sceneToMapX(pos.x()), sceneToMapY(pos.y()));
}

QPointF QMeshPlot::sceneToMap(qreal x, qreal y) const
{
    return QPointF(sceneToMapX(x), sceneToMapY(y));
}

qreal QMeshPlot::sceneToMapX (qreal x) const
{
    return width() * (x - drawRect_.x()) / drawRect_.width();
}

qreal QMeshPlot::sceneToMapY (qreal y) const
{
    return height() * (1. - (y - drawRect_.y()) / drawRect_.height());
}

QPointF QMeshPlot::mapToScene(const QPointF &pos) const
{
    return QPointF(mapToSceneX(pos.x()), mapToSceneY(pos.y()));
}

QPointF QMeshPlot::mapToScene(qreal x, qreal y) const
{
    return QPointF(mapToSceneX(x), mapToSceneY(y));
}

qreal QMeshPlot::mapToSceneX (qreal x) const
{
    return drawRect_.x() + drawRect_.width() * x / width();
}

qreal QMeshPlot::mapToSceneY (qreal y) const
{
    return drawRect_.y() + drawRect_.height() * (height() - y) / height();
}

void QMeshPlot::drawBackground()
{
    QPainter painter;
    painter.begin(this);
    painter.fillRect(QRectF(0, 0, width(), height()), backgroundColor_);
    painter.end();
}

void QMeshPlot::drawAxes()
{
    QPainter painter;
    painter.begin(this);
    painter.setRenderHint(QPainter::Antialiasing);
    painter.setPen(QPen(axesColor_));
    painter.drawLine(0, sceneToMapY(0), width(), sceneToMapY(0));
    painter.drawLine(sceneToMapX(0), 0, sceneToMapX(0), height());

    QColor auxAxesColor = axesColor_;
    auxAxesColor.setAlpha(0x20);

    double xtick = ceil (drawRect_.x() / tickDx_) * tickDx_;
    for (int i = 0; i < countTickX_; i++)
    {
        if (fabs (xtick) > 1e-9)
        {
            painter.setPen(QPen(auxAxesColor, 1, Qt::DashLine));
            painter.drawLine(sceneToMapX(xtick), 0, sceneToMapX(xtick), height());
        }
        xtick += tickDx_;
    }

    double ytick = ceil (drawRect_.y() / tickDy_) * tickDy_;
    for (int i = 0; i < countTickY_; i++)
    {
        if (fabs (ytick) > 1e-9)
        {
            painter.setPen(QPen(auxAxesColor, 1, Qt::DashLine));
            painter.drawLine(0, sceneToMapY(ytick), width(), sceneToMapY(ytick));
        }
        ytick += tickDy_;
    }

    painter.end();
}

void QMeshPlot::drawBorders()
{
    QPainter painter;
    painter.begin(this);

    double ytick = ceil (drawRect_.y() / tickDy_) * tickDy_;
    int maxYtw = 0;
    for (int i = 0; i < countTickY_; i++)
    {
        if (fabs (ytick) < 1e-9)
            ytick = 0;
        std::stringstream tickStream;
        tickStream << ytick;

        QFontMetrics metrics = QFontMetrics(font());
        int tw = metrics.width(tickStream.str().c_str());
        if (tw > maxYtw)
            maxYtw = tw;
        ytick += tickDy_;
    }
    maxYtw += 10;

    setLeftMargin(maxYtw);

    painter.fillRect(QRectF(0, height() - bottomMargin_, width(), bottomMargin_),
                     QColor(0xff, 0xff, 0xff));

    double xtick = ceil (drawRect_.x() / tickDx_) * tickDx_;
    for (int i = 0; i < countTickX_; i++)
    {
        if (fabs (xtick) < 1e-9)
            xtick = 0;
        std::stringstream tickStream;
        tickStream << xtick;

        QFontMetrics metrics = QFontMetrics(font());
        int tw = metrics.width(tickStream.str().c_str());

        painter.setPen(QPen(QColor(0x00, 0x00, 0x00)));
        if (sceneToMapX(xtick) - 0.5 * tw > leftMargin_)
            painter.drawText(QPointF(sceneToMapX(xtick) - 0.5 * tw, height() - 9), tickStream.str().c_str());

        painter.setPen(QPen(axesColor_));
        painter.drawLine(sceneToMapX(xtick), height() - bottomMargin_ - 10,
                         sceneToMapX(xtick), height() - bottomMargin_);
        xtick += tickDx_;
    }
    painter.fillRect(QRectF(0, 0, leftMargin_, height() - bottomMargin_),
                     QColor(0xff, 0xff, 0xff));

    ytick = ceil (drawRect_.y() / tickDy_) * tickDy_;
    for (int i = 0; i < countTickY_; i++)
    {
        if (fabs (ytick) < 1e-9)
            ytick = 0;
        std::stringstream tickStream;
        tickStream << ytick;

        QFontMetrics metrics = QFontMetrics(font());
        int tw = metrics.width(tickStream.str().c_str());

        painter.setPen(QPen(QColor(0x00, 0x00, 0x00)));
        if (sceneToMapY(ytick) < height() - bottomMargin_)
        {
            painter.drawText(QPointF(leftMargin_ - tw - 5, sceneToMapY(ytick) + 3),
                             tickStream.str().c_str());
            painter.setPen(QPen(axesColor_));
            painter.drawLine(leftMargin_, sceneToMapY(ytick), leftMargin_ + 10, sceneToMapY(ytick));
        }
        ytick += tickDy_;
    }

    painter.end();
}

void QMeshPlot::drawItems()
{
    QPainter painter;
    painter.begin(this);
    painter.save();
    painter.setRenderHint(QPainter::Antialiasing);
    painter.setBrush(foregroundColor_);
    painter.translate(dx_, dy_);
    foreach (QMeshItem *item, items_)
        item->draw(painter, scaleX_, scaleY_);
    painter.restore();
    painter.end();
}

void QMeshPlot::setBackgroundColor(const QColor &color)
{
    backgroundColor_ = color;
}

void QMeshPlot::setForegroundColor(const QColor &color)
{
    foregroundColor_ = color;
}

void QMeshPlot::setPenColor(const QColor &color)
{
    penColor_ = color;
}

void QMeshPlot::setAxesColor(const QColor &color)
{
    axesColor_ = color;
}

void QMeshPlot::setBottomMargin(int margin)
{
    bottomMargin_ = margin;
}

void QMeshPlot::setLeftMargin(int margin)
{
    leftMargin_ = margin;
}
