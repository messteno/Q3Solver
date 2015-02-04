#include <math.h>

#include "q3sceletonitem.h"
#include "q3point.h"

const int Q3Point::PointSize = 5;

Q3Point::Q3Point() :
    Q3SceletonItem(Q3SceletonItem::Point)
{
}

Q3Point::Q3Point(const Q3Point &q3point) :
    Q3SceletonItem(Q3SceletonItem::Point)
{
    point_ = q3point.point_;
}

Q3Point::Q3Point(const QPointF &point) :
    Q3SceletonItem(Q3SceletonItem::Point)
{
    point_ = point;
}

Q3Point::~Q3Point()
{

}

void Q3Point::draw(Q3Painter &painter)
{
    qreal scaleX = painter.sx();
    qreal scaleY = painter.sy();
    painter.drawEllipse(QPointF(point_.x() * scaleX,
                                point_.y() * scaleY),
                        PointSize, PointSize);
}

void Q3Point::move(const QPointF diff)
{
    point_ += diff;
}

qreal Q3Point::distanceTo(const QPointF &pos) const
{
    QPointF diff = point_ - pos;
    return sqrt(diff.x() * diff.x() + diff.y() * diff.y());
}

qreal Q3Point::distanceFromBoundaryTo(const QPointF &pos) const
{
    return distanceTo(pos);
}

QRectF Q3Point::boundingRect() const
{
    return QRectF(point_.x() - 0.5 * PointSize, point_.y() - 0.5 * PointSize,
                  PointSize, PointSize);
}

qreal Q3Point::x()
{
    return point_.x();
}

qreal Q3Point::y()
{
    return point_.y();
}

QPointF Q3Point::point()
{
    return point_;
}
