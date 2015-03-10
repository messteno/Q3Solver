#include <math.h>

#include "q3circle.h"
#include "q3itemvisitor.h"

const qreal Q3Circle::MinRadius = 1e-5;

Q3Circle::Q3Circle(const QPointF &center, qreal radius) :
    Q3SceletonItem(Q3SceletonItem::Circle),
    center_(center),
    radius_(radius)
{
    setResizable(true);
}

Q3Circle::~Q3Circle()
{

}

void Q3Circle::draw(Q3Painter &painter)
{
    painter.save();
    painter.setBrush(Qt::NoBrush);
    qreal scaleX = painter.sx();
    qreal scaleY = painter.sy();
    painter.drawEllipse(QPointF(center_.x() * scaleX,
                                center_.y() * scaleY),
                        radius_ * scaleX, radius_ * scaleY);
    painter.drawLine(QPointF(center_.x() * scaleX, center_.y() * scaleY - 5),
                     QPointF(center_.x() * scaleX, center_.y() * scaleY + 5));
    painter.drawLine(QPointF(center_.x() * scaleX - 5, center_.y() * scaleY),
                     QPointF(center_.x() * scaleX + 5, center_.y() * scaleY));
    painter.restore();
}

qreal Q3Circle::distanceTo(const QPointF &pos) const
{
    return sqrt((pos.x() - center_.x()) * (pos.x() - center_.x()) +
                (pos.y() - center_.y()) * (pos.y() - center_.y()));
}

qreal Q3Circle::distanceFromBoundaryTo(const QPointF &pos) const
{
    return fabs(radius_ - distanceTo(pos));
}

QRectF Q3Circle::boundingRect() const
{
    return QRectF(center_.x() - radius_, center_.y() - radius_,
                  2 * radius_, 2 * radius_);
}

void Q3Circle::move(const QPointF diff)
{
    center_ += diff;
}

void Q3Circle::resize(const QPointF from, const QPointF to)
{
    qreal radius = sqrt((to.x() - center_.x()) * (to.x() - center_.x()) +
                        (to.y() - center_.y()) * (to.y() - center_.y()));
    if (radius < Q3Circle::MinRadius)
        return;
    setRadius(radius);
}

QPointF Q3Circle::center()
{
    return center_;
}

void Q3Circle::setCenter(const QPointF &center)
{
    center_ = center;
}

qreal Q3Circle::radius()
{
    return radius_;
}

void Q3Circle::setRadius(qreal radius)
{
    if (radius < Q3Circle::MinRadius)
        radius_ = Q3Circle::MinRadius;
    else
        radius_ = radius;
}

bool Q3Circle::accept(Q3ItemVisitor &visitor, Q3SceletonItem *item)
{
    return item->accept(visitor, this);
}

bool Q3Circle::accept(Q3ItemVisitor &visitor, Q3Point *point)
{
    return visitor.visit(point, this);
}

bool Q3Circle::accept(Q3ItemVisitor &visitor, Q3PointConnection *conn)
{
    return visitor.visit(conn, this);
}

bool Q3Circle::accept(Q3ItemVisitor &visitor, Q3Circle *circle)
{
    return visitor.visit(circle, this);
}

bool Q3Circle::accept(Q3ItemVisitor &visitor)
{
    visitor.visit(this);
}
