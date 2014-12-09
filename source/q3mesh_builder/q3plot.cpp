#include <QDebug>
#include <QPainter>
#include <QWheelEvent>
#include <QFont>
#include <string>
#include <iostream>
#include <sstream>
#include <qmath.h>

#include "q3plot.h"

Q3Plot::Q3Plot(QWidget *parent) :
    QWidget(parent),
    sceneRect_(-2, -2, 13, 8),
    drawRect_(sceneRect_),
    scaleX_(1.),
    scaleY_(-1.),
    tickDx_(1),
    tickDy_(1),
    countTickX_(10),
    countTickY_(10),
    clickedPos_(0, 0),
    backgroundColor_(0x00, 0x16, 0x1c),
    foregroundColor_(0x1d, 0xd3, 0xf3, 0xa0),
    axesColor_(0xff, 0xff, 0xff),
    bottomMargin_(25),
    leftMargin_(20),
    wheelDelta_(0)
{
    setMouseTracking(true);
}

Q3Plot::~Q3Plot()
{
}

void Q3Plot::resizeEvent(QResizeEvent *event)
{
    updateScene();
}

void Q3Plot::paintEvent(QPaintEvent *event)
{
    drawBackground();
    drawAxes();
    drawBorders();
}

void Q3Plot::wheelEvent(QWheelEvent *event)
{
    wheelDelta_ += event->delta();
    int numSteps = wheelDelta_ / 15 / 8;
    if (numSteps == 0)
    {
        event->ignore();
        return;
    }

    wheelDelta_ = 0;

    qreal sc = pow(0.9, numSteps);
    QPointF scenePos = mapToScene(event->pos());
    qreal sxl = scenePos.x() - drawRect_.x();
    qreal syl = scenePos.y() - drawRect_.y();
    qreal sxr = drawRect_.x() + drawRect_.width() - scenePos.x();
    qreal syr = drawRect_.y() + drawRect_.height() - scenePos.y();
    sceneRect_.setTopLeft(QPointF(scenePos.x() - sxl * sc,
                                  scenePos.y() - syl * sc));
    sceneRect_.setBottomRight(QPointF(scenePos.x() + sxr * sc,
                                      scenePos.y() + syr * sc));
    updateScene();
    repaint();
}

void Q3Plot::mouseReleaseEvent(QMouseEvent *event)
{
    if (mousePos_.isNull())
    {
        clickedPos_ = event->pos();
        emit mouseClicked(this);
    }
    else
    {
        mousePos_ = QPointF();
    }
}

void Q3Plot::mouseMoveEvent(QMouseEvent *event)
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

void Q3Plot::updateScene()
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

QPointF Q3Plot::sceneToMap(const QPointF &pos) const
{
    return QPointF(sceneToMapX(pos.x()), sceneToMapY(pos.y()));
}

QPointF Q3Plot::sceneToMap(qreal x, qreal y) const
{
    return QPointF(sceneToMapX(x), sceneToMapY(y));
}

qreal Q3Plot::sceneToMapX(qreal x) const
{
    return width() * (x - drawRect_.x()) / drawRect_.width();
}

qreal Q3Plot::sceneToMapY(qreal y) const
{
    return height() * (1. - (y - drawRect_.y()) / drawRect_.height());
}

QPointF Q3Plot::mapToScene(const QPointF &pos) const
{
    return QPointF(mapToSceneX(pos.x()), mapToSceneY(pos.y()));
}

QPointF Q3Plot::mapToScene(qreal x, qreal y) const
{
    return QPointF(mapToSceneX(x), mapToSceneY(y));
}

qreal Q3Plot::mapToSceneX(qreal x) const
{
    return drawRect_.x() + drawRect_.width() * x / width();
}

qreal Q3Plot::mapToSceneY (qreal y) const
{
    return drawRect_.y() + drawRect_.height() * (height() - y) / height();
}

void Q3Plot::drawBackground()
{
    QPainter painter;
    painter.begin(this);
    painter.fillRect(QRectF(0, 0, width(), height()), backgroundColor_);
    painter.end();
}

void Q3Plot::drawAxes()
{
    QPainter painter;
    painter.begin(this);
    painter.setRenderHint(QPainter::Antialiasing);
    painter.setPen(QPen(axesColor_));
    painter.drawLine(0, sceneToMapY(0), width(), sceneToMapY(0));
    painter.drawLine(sceneToMapX(0), 0, sceneToMapX(0), height());

    QColor auxAxesColor = axesColor_;
    auxAxesColor.setAlpha(0x20);

    double xtick = ceil(drawRect_.x() / tickDx_) * tickDx_;
    for (int i = 0; i < countTickX_; ++i)
    {
        if (fabs (xtick) > 1e-9)
        {
            painter.setPen(QPen(auxAxesColor, 1, Qt::DashLine));
            painter.drawLine(sceneToMapX(xtick), 0,
                             sceneToMapX(xtick), height());
        }
        xtick += tickDx_;
    }

    double ytick = ceil(drawRect_.y() / tickDy_) * tickDy_;
    for (int i = 0; i < countTickY_; ++i)
    {
        if (fabs (ytick) > 1e-9)
        {
            painter.setPen(QPen(auxAxesColor, 1, Qt::DashLine));
            painter.drawLine(0, sceneToMapY(ytick),
                             width(), sceneToMapY(ytick));
        }
        ytick += tickDy_;
    }

    painter.end();
}

void Q3Plot::drawBorders()
{
    QPainter painter;
    painter.begin(this);

    double ytick = ceil(drawRect_.y() / tickDy_) * tickDy_;
    int maxYtw = 0;
    for (int i = 0; i < countTickY_; ++i)
    {
        if (fabs(ytick) < 1e-9)
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

    painter.fillRect(QRectF(0, height() - bottomMargin_,
                            width(), bottomMargin_),
                     QColor(0xff, 0xff, 0xff));

    double xtick = ceil(drawRect_.x() / tickDx_) * tickDx_;
    for (int i = 0; i < countTickX_; ++i)
    {
        if (fabs (xtick) < 1e-9)
            xtick = 0;
        std::stringstream tickStream;
        tickStream << xtick;

        QFontMetrics metrics = QFontMetrics(font());
        int tw = metrics.width(tickStream.str().c_str());

        painter.setPen(QPen(QColor(0x00, 0x00, 0x00)));
        if (sceneToMapX(xtick) - 0.5 * tw > leftMargin_)
            painter.drawText(QPointF(sceneToMapX(xtick) - 0.5 * tw,
                                     height() - 9),
                             tickStream.str().c_str());

        painter.setPen(QPen(axesColor_));
        painter.drawLine(sceneToMapX(xtick), height() - bottomMargin_ - 10,
                         sceneToMapX(xtick), height() - bottomMargin_);
        xtick += tickDx_;
    }
    painter.fillRect(QRectF(0, 0, leftMargin_, height() - bottomMargin_),
                     QColor(0xff, 0xff, 0xff));

    ytick = ceil (drawRect_.y() / tickDy_) * tickDy_;
    for (int i = 0; i < countTickY_; ++i)
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
            painter.drawText(QPointF(leftMargin_ - tw - 5,
                                     sceneToMapY(ytick) + 3),
                             tickStream.str().c_str());
            painter.setPen(QPen(axesColor_));
            painter.drawLine(leftMargin_, sceneToMapY(ytick),
                             leftMargin_ + 10, sceneToMapY(ytick));
        }
        ytick += tickDy_;
    }

    painter.end();
}

void Q3Plot::setBackgroundColor(const QColor &color)
{
    backgroundColor_ = color;
}

void Q3Plot::setForegroundColor(const QColor &color)
{
    foregroundColor_ = color;
}

void Q3Plot::setPenColor(const QColor &color)
{
    penColor_ = color;
}

void Q3Plot::setAxesColor(const QColor &color)
{
    axesColor_ = color;
}

void Q3Plot::setBottomMargin(int margin)
{
    bottomMargin_ = margin;
}

void Q3Plot::setLeftMargin(int margin)
{
    leftMargin_ = margin;
}

QPointF Q3Plot::getClickedScenePosition(bool snapToGrid)
{
    QPointF pos = mapToScene(clickedPos_);
    if (!snapToGrid)
        return pos;
    double x = round(pos.x() / tickDx_) * tickDx_;
    double y = round(pos.y() / tickDy_) * tickDy_;
    if (fabs(x - pos.x()) < 0.1 * tickDx_ &&
        fabs(y - pos.y()) < 0.1 * tickDy_)
    {
        return QPointF(x, y);
    }
    return pos;
}
