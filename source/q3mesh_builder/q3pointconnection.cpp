#include <math.h>

#include <QDebug>
#include <QVector2D>

#include "q3sceletonitem.h"
#include "q3pointconnection.h"
#include "q3itemvisitor.h"

Q3PointConnection::Q3PointConnection(Q3Point *a, Q3Point *b) :
    Q3SceletonItem(Q3SceletonItem::PointConnection),
    a_(a),
    b_(b)
{
}

Q3PointConnection::~Q3PointConnection()
{

}

void Q3PointConnection::draw(Q3Painter &painter)
{
    qreal scaleX = painter.sx();
    qreal scaleY = painter.sy();
    painter.drawLine(a_->x() * scaleX, a_->y() * scaleY,
                     b_->x() * scaleX, b_->y() * scaleY);
}

qreal Q3PointConnection::distanceTo(const QPointF &pos) const
{
    qreal len = QLineF(a_->point(), b_->point()).length();
    if (fabs(len) < 1e-16)
        return a_->distanceTo(pos);
    qreal param = ((b_->x() - a_->x()) * (pos.x() - a_->x()) +
                   (b_->y() - a_->y()) * (pos.y() - a_->y())) / len / len;
    if (param < 0)
        return a_->distanceTo(pos);
    else if (param > 1)
        return b_->distanceTo(pos);
    Q3Point project(a_->point() + param * (b_->point() - a_->point()));
    return project.distanceTo(pos);
}

qreal Q3PointConnection::distanceFromBoundaryTo(const QPointF &pos) const
{
    return distanceTo(pos);
}

QRectF Q3PointConnection::boundingRect() const
{
    qreal x1 = a_->x();
    qreal x2 = b_->x();
    qreal y1 = a_->y();
    qreal y2 = b_->y();

    return QRectF(QPointF(qMin(x1, x2), qMax(y1, y2)),
                  QPointF(qMax(x1, x2), qMin(y1, y2)));
}

void Q3PointConnection::move(const QPointF diff)
{
    if (!a_->isSelected())
        a_->move(diff);
    if (!b_->isSelected())
        b_->move(diff);
}

void Q3PointConnection::setA(Q3Point *a)
{
    a_ = a;
}

void Q3PointConnection::setB(Q3Point *b)
{
    b_ = b;
}

Q3Point *Q3PointConnection::a()
{
    return a_;
}

Q3Point *Q3PointConnection::b()
{
    return b_;
}

bool Q3PointConnection::accept(Q3ItemVisitor &visitor, Q3SceletonItem *item)
{
    return item->accept(visitor, this);
}

bool Q3PointConnection::accept(Q3ItemVisitor &visitor, Q3Point *point)
{
    return visitor.visit(point, this);
}

bool Q3PointConnection::accept(Q3ItemVisitor &visitor, Q3PointConnection *conn)
{
    return visitor.visit(this, conn);
}

bool Q3PointConnection::accept(Q3ItemVisitor &visitor, Q3Circle *circle)
{
    return visitor.visit(this, circle);
}

bool Q3PointConnection::accept(Q3ItemVisitor &visitor)
{
    return visitor.visit(this);
}
