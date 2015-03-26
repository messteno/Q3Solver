#include <QVector2D>
#include <QDebug>
#include <qmath.h>

#include "q3meshnode.h"
#include "q3meshedge.h"
#include "q3meshtriangle.h"

Q3MeshEdge::Q3MeshEdge(Q3MeshNode *a, Q3MeshNode *b) :
    a_(a),
    b_(b)
{
    Q_ASSERT(a_);
    Q_ASSERT(b_);

    center_ = (*a_ + *b_) / 2.;
    length_ = QVector2D(*a_ - *b_).length();

    vertices_ << a << b;
}

Q3MeshNode *Q3MeshEdge::a() const
{
    return a_;
}

Q3MeshNode *Q3MeshEdge::b() const
{
    return b_;
}

Q3MeshNode *Q3MeshEdge::nodeAdjacentTo(Q3MeshEdge *edge)
{
    if (edge->a() == a_ || edge->b() == a_)
        return a_;
    if (edge->a() == b_ || edge->b() == b_)
        return b_;
    return NULL;
}

QPointF Q3MeshEdge::center() const
{
    return center_;
}

qreal Q3MeshEdge::length() const
{
    return length_;
}

void Q3MeshEdge::addAdjacentTriangle(Q3MeshTriangle *triangle)
{
    if (adjacentTriangles_.contains(triangle))
        return;

    foreach (Q3MeshTriangle *adjacentTriangle, adjacentTriangles_)
    {
        // Сначала обновим центр отрезка, чтобы для треугольников
        // получились правильные расстояния
        center_ = this->cross(triangle->center(), adjacentTriangle->center());
        triangle->addAdjacentTriangle(adjacentTriangle, this);
        adjacentTriangle->addAdjacentTriangle(triangle, this);
    }
    adjacentTriangles_.append(triangle);

    foreach (Q3MeshEdge *edge, triangle->edges())
        addAdjacentEdge(edge);
}

void Q3MeshEdge::addAdjacentEdge(Q3MeshEdge *edge)
{
    if (adjacentEdges_.contains(edge) || (edge == this))
        return;

    adjacentEdges_.append(edge);

    Q3MeshNode *node = this->nodeAdjacentTo(edge);
    Q_ASSERT(node);

    QPointF a = *node;
    QPointF b = *((this->a() == node) ? this->b() : this->a());
    QPointF c = *((edge->a() == node) ? edge->b() : edge->a());

    qreal scalar = QVector2D::dotProduct(QVector2D(b - a), QVector2D(c - a));
    qreal cosin = scalar / this->length() / edge->length();
    qreal cotangent = cosin / qSqrt(1 - cosin * cosin);

    adjacentCotangents_.append(cotangent);
}

QList<Q3MeshNode *> Q3MeshEdge::vertices() const
{
    return vertices_;
}

QList<Q3MeshEdge *> Q3MeshEdge::adjacentEdges() const
{
    return adjacentEdges_;
}

QList<Q3MeshTriangle *> Q3MeshEdge::adjacentTriangles() const
{
    return adjacentTriangles_;
}

QList<qreal> Q3MeshEdge::adjacentCotangents() const
{
    return adjacentCotangents_;
}

QPointF Q3MeshEdge::cross(const QPointF &p1, const QPointF &p2)
{
    qreal x1 = b_->x() - a_->x();
    qreal y1 = b_->y() - a_->y();
    qreal x2 = p1.x() - p2.x();
    qreal y2 = p1.y() - p2.y();
    qreal x3 = p1.x() - a_->x();
    qreal y3 = p1.y() - a_->y();

    // |x1 x2| x3
    // |y1 y2| y3

    qreal d = x1 * y2 - x2 * y1;
    qreal d1 = x3 * y2 - x2 * y3;

    qreal t = d1 / d;

    return *a_ + t * QPointF(*b_ - *a_);
}

QVector2D Q3MeshEdge::normalVector()
{
    QVector2D edge(*b_ - *a_);
    QVector2D normal(edge.y(), -edge.x());
    normal.normalize();
    return normal;
}
