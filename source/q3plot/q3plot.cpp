#include <QDebug>
#include <q3painter.h>
#include <QWheelEvent>
#include <QFont>
#include <string>
#include <iostream>
#include <sstream>
#include <qmath.h>

#include "q3plot.h"

const QColor Q3Plot::DefaultBackgroundColor = QColor(0x00, 0x16, 0x1c);
const QColor Q3Plot::DefaultForegroundColor = QColor(0x1d, 0xd3, 0xf3, 0xa0);
const QColor Q3Plot::DefaultAxesColor = QColor(0xff, 0xff, 0xff);
const QColor Q3Plot::DefaultPenColor = QColor(0xff, 0xff, 0xff);
const QColor Q3Plot::DefaultTextColor = QColor(0x00, 0x00, 0x00);
const QColor Q3Plot::DefaultBorderColor = QColor(0xff, 0xff, 0xff);

const int Q3Plot::MinTickCount = 8;

Q3Plot::Q3Plot(QWidget *parent) :
    QWidget(parent),
    sceneRect_(-3, -3, 8, 8),
    drawRect_(sceneRect_),
    scaleX_(1.),
    scaleY_(-1.),
    tickDx_(1),
    tickDy_(1),
    countTickX_(10),
    countTickY_(10),
    backgroundColor_(DefaultBackgroundColor),
    foregroundColor_(DefaultForegroundColor),
    penColor_(DefaultPenColor),
    axesColor_(DefaultAxesColor),
    borderColor_(DefaultBorderColor),
    textColor_(DefaultTextColor),
    bottomMargin_(25),
    leftMargin_(20),
    wheelDelta_(0),
    snapToGrid_(true)
{
    setMouseTracking(true);
    setMinimumWidth(100);
    setMinimumHeight(100);
}

Q3Plot::~Q3Plot()
{
}

void Q3Plot::resizeEvent(QResizeEvent *event)
{
    Q_UNUSED(event);

    updateScene();
}

void Q3Plot::paintEvent(QPaintEvent *event)
{
    Q_UNUSED(event);

    Q3Painter painter;
    painter.begin(this);
    drawBackground(painter);
    drawAxes(painter);
    drawDrawables(painter);
    drawBorders(painter);
    painter.end();
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
    this->setFocus();
    QPointF sceneClickedPos = mapToScene(event->pos());
    if (leftButtonMousePos_.isNull())
    {
        emit mouseClicked(event, sceneClickedPos);
    }
    else
    {
        leftButtonMousePos_ = QPointF();
        emit mouseDropped(sceneClickedPos);
    }
}

void Q3Plot::keyReleaseEvent(QKeyEvent *event)
{
    emit keyReleased(event);
}

void Q3Plot::keyPressEvent(QKeyEvent *event)
{
    emit keyReleased(event);
}

qreal Q3Plot::sx() const
{
    return scaleX_;
}

qreal Q3Plot::sy() const
{
    return scaleY_;
}

void Q3Plot::mouseMoveEvent(QMouseEvent *event)
{
    this->setFocus();
    if (event->buttons() & Qt::LeftButton)
    {
        if (leftButtonMousePos_.isNull())
        {
            leftButtonMousePos_ = event->pos();
        }
        else
        {
            QPointF oldScenePos = mapToScene(leftButtonMousePos_);
            QPointF newScenePos = mapToScene(event->pos());
            emit mouseDragged(oldScenePos, newScenePos);
            leftButtonMousePos_ = event->pos();
        }
        mousePos_ = event->pos();
    }
    else if (!event->buttons())
    {
        QPointF oldScenePos = mapToScene(mousePos_);
        QPointF newScenePos = mapToScene(event->pos());
        emit mouseMoved(oldScenePos, newScenePos);
        mousePos_ = event->pos();
    }
}

void Q3Plot::moveScene(const QPointF diff)
{
    sceneRect_ = QRectF(sceneRect_.x() - diff.x(),
                        sceneRect_.y() - diff.y(),
                        sceneRect_.width(),
                        sceneRect_.height());
    updateScene();
}

bool Q3Plot::snapToGrid() const
{
    return snapToGrid_;
}

void Q3Plot::setSnapToGrid(bool snapTogrid)
{
    snapToGrid_ = snapTogrid;
}

void Q3Plot::setSceneRect(const QRectF &sceneRect)
{
    sceneRect_ = sceneRect;
    updateScene();
    int bw = borderWidth();
    sceneRect_.setLeft(sceneRect_.left()
                       - (bw + 30.) * sceneRect_.width() / width());
    sceneRect_.setRight(sceneRect_.right()
                        + 20. * sceneRect_.width() / width());
    sceneRect_.setTop(sceneRect_.top()
                      - (bottomMargin_ + 20.) * sceneRect_.height() / height());
    sceneRect_.setBottom(sceneRect_.bottom()
                         + 20. * sceneRect_.height() / height());
    updateScene();
}

QRectF Q3Plot::sceneRect() const
{
    return sceneRect_;
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

    dx_ = sceneToMapX(0);
    dy_ = sceneToMapY(0);

    tickDx_ = pow(10, (int) (log(drawRect_.width()) / log(10.)));
    tickDy_ = pow(10, (int) (log(drawRect_.height()) / log(10.)));

    countTickX_ = ceil(drawRect_.width() / tickDx_);
    countTickY_ = ceil(drawRect_.height() / tickDy_);

    while (countTickX_ < MinTickCount && tickDx_ > 1e-10)
    {
        tickDx_ /= 2.;
        countTickX_ = ceil(drawRect_.width() / tickDx_);
    }

    while (countTickY_ < MinTickCount && tickDy_ > 1e-10)
    {
        tickDy_ /= 2.;
        countTickY_ = ceil(drawRect_.height() / tickDy_);
    }

    if ((tickDx_ / tickDy_ < 2) && (tickDy_ / tickDx_ < 2))
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

void Q3Plot::drawBackground(Q3Painter &painter)
{
    painter.fillRect(QRectF(0, 0, width(), height()), backgroundColor_);
}

void Q3Plot::drawAxes(Q3Painter &painter)
{
    painter.setRenderHint(Q3Painter::Antialiasing, false);
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
}

int Q3Plot::borderWidth()
{
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
    return maxYtw;
}

void Q3Plot::drawBorders(Q3Painter &painter)
{
    int maxYtw = borderWidth();
    setLeftMargin(maxYtw);

    painter.setRenderHint(Q3Painter::Antialiasing, false);

    painter.fillRect(QRectF(0, height() - bottomMargin_,
                            width(), bottomMargin_),
                     borderColor_);
    double xtick = ceil(drawRect_.x() / tickDx_) * tickDx_;
    for (int i = 0; i < countTickX_; ++i)
    {
        if (fabs (xtick) < 1e-9)
            xtick = 0;
        std::stringstream tickStream;
        tickStream << xtick;

        QFontMetrics metrics = QFontMetrics(font());
        int tw = metrics.width(tickStream.str().c_str());

        painter.setPen(textColor_);
        if (sceneToMapX(xtick) - 0.5 * tw > leftMargin_)
            painter.drawText(QPointF(sceneToMapX(xtick) - 0.5 * tw,
                                     height() - 9),
                             tickStream.str().c_str());

        painter.setPen(axesColor_);
        painter.drawLine(sceneToMapX(xtick), height() - bottomMargin_ - 10,
                         sceneToMapX(xtick), height() - bottomMargin_);
        xtick += tickDx_;
    }
    painter.setPen(QPen(Qt::black, 1));
    painter.drawLine(leftMargin_, height() - bottomMargin_,
                     width(), height() - bottomMargin_);

    painter.fillRect(QRectF(0, 0, leftMargin_, height() - bottomMargin_),
                     borderColor_);
    qreal ytick = ceil (drawRect_.y() / tickDy_) * tickDy_;
    for (int i = 0; i < countTickY_; ++i)
    {
        if (fabs (ytick) < 1e-9)
            ytick = 0;
        std::stringstream tickStream;
        tickStream << ytick;

        QFontMetrics metrics = QFontMetrics(font());
        int tw = metrics.width(tickStream.str().c_str());

        painter.setPen(textColor_);
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
    painter.setPen(QPen(Qt::black, 1));
    painter.drawLine(leftMargin_, 0,
                     leftMargin_, height() - bottomMargin_);
}

void Q3Plot::drawDrawables(Q3Painter &painter)
{
    painter.save();

    painter.setPen(penColor_);
    painter.setRenderHint(Q3Painter::Antialiasing);
    painter.setBrush(foregroundColor_);
    painter.translate(dx_, dy_);
    painter.doScale(scaleX_, scaleY_);
    foreach (Q3PlotDrawable *item, drawables_)
        item->draw(painter);
    painter.restore();
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

void Q3Plot::setBorderColor(const QColor &borderColor)
{
    borderColor_ = borderColor;
}

void Q3Plot::setTextColor(const QColor &textColor)
{
    textColor_ = textColor;
}

void Q3Plot::setBottomMargin(int margin)
{
    bottomMargin_ = margin;
}

void Q3Plot::setLeftMargin(int margin)
{
    leftMargin_ = margin;
}

void Q3Plot::addDrawable(Q3PlotDrawable *item)
{
    if (!drawables_.contains(item))
        drawables_.append(item);
}

void Q3Plot::removeDrawable(Q3PlotDrawable *item)
{
    if (drawables_.contains(item))
        drawables_.removeAll(item);
}

void Q3Plot::clearDrawable()
{
    drawables_.clear();
}

QPointF Q3Plot::snapScenePosToGrid (const QPointF pos)
{
    if (!snapToGrid_)
        return pos;

    double x = round(pos.x() / tickDx_) * tickDx_;
    double y = round(pos.y() / tickDy_) * tickDy_;
    if (fabs(x - pos.x()) < 0.3 * tickDx_ &&
        fabs(y - pos.y()) < 0.3 * tickDy_)
    {
        return QPointF(x, y);
    }
    return pos;
}
