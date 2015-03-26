#include <math.h>

#include "q3sceletonitem.h"
#include "q3point.h"
#include "q3itemvisitor.h"

#include <QLineEdit>
#include <QLabel>

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
    moved_ = true;
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
    // return QRectF(point_.x() - 0.5 * PointSize, point_.y() - 0.5 * PointSize,
    //              PointSize, PointSize);
    return QRectF(point_.x(), point_.y(), 0, 0);
}

qreal Q3Point::x()
{
    return point_.x();
}

qreal Q3Point::y()
{
    return point_.y();
}

void Q3Point::setX(qreal x)
{
    point_.setX(x);
}

void Q3Point::setY(qreal y)
{
    point_.setY(y);
}

QPointF Q3Point::point()
{
    return point_;
}

bool Q3Point::accept(Q3ItemVisitor &visitor, Q3SceletonItem *item)
{
    return item->accept(visitor, this);
}

bool Q3Point::accept(Q3ItemVisitor &visitor, Q3Point *point)
{
    return visitor.visit(this, point);
}

bool Q3Point::accept(Q3ItemVisitor &visitor, Q3PointConnection *conn)
{
    return visitor.visit(this, conn);
}

bool Q3Point::accept(Q3ItemVisitor &visitor, Q3Circle *circle)
{
    return visitor.visit(this, circle);
}

bool Q3Point::accept(Q3ItemVisitor &visitor)
{
    visitor.visit(this);
}

void Q3Point::editUI(QFormLayout *layout)
{
    QLineEdit *xEdit = new QLineEdit;
    layout->addRow("x:", xEdit);
}

QString Q3Point::toString()
{
    return "(" + QString::number(point_.x()) + " , "
            + QString::number(point_.y()) + ")";
}

QString Q3Point::typeToString()
{
    return "Точка";
}
