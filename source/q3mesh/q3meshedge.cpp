#include <QVector2D>
#include <QVector3D>
#include <QDebug>
#include <qmath.h>

#include "q3meshnode.h"
#include "q3meshedge.h"
#include "q3meshtriangle.h"

Q3MeshEdge::Q3MeshEdge(Q3MeshNode *a, Q3MeshNode *b,
                       Q3Boundary *boundary, int id) :
    a_(a),
    b_(b),
    id_(id),
    boundary_(boundary),
    velocity_(0, 0),
    pressure_(0),
    adjacentSquare_(0)
{
    Q_ASSERT(a_);
    Q_ASSERT(b_);

    vertices_ << a << b;

    center_ = (*a_ + *b_) / 2.;
    length_ = QVector2D(*a_ - *b_).length();

    if (boundary_)
    {
        a_->setBoundary(true);
        b_->setBoundary(true);
    }
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

qreal Q3MeshEdge::adjacentSquare() const
{
    return adjacentSquare_;
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

    QPointF triangleCenter = triangle->center();
    adjacentSquare_ += 0.5 * qAbs(QVector3D::crossProduct(
                                      QVector3D(triangleCenter - *a_),
                                      QVector3D(*b_ - *a_)).length());
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

QList<Q3MeshNode *> &Q3MeshEdge::vertices()
{
    return vertices_;
}

QList<Q3MeshEdge *> &Q3MeshEdge::adjacentEdges()
{
    return adjacentEdges_;
}

QList<Q3MeshTriangle *> &Q3MeshEdge::adjacentTriangles()
{
    return adjacentTriangles_;
}

QList<qreal> &Q3MeshEdge::adjacentCotangents()
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

QVector2D Q3MeshEdge::normalVector() const
{
    QVector2D edge(*b_ - *a_);
    QVector2D normal(edge.y(), - edge.x());
    normal.normalize();
    return normal;
}

QVector2D Q3MeshEdge::velocity() const
{
    return velocity_;
}

void Q3MeshEdge::setVelocity(const QVector2D &velocity)
{
    velocity_ = velocity;
}

qreal Q3MeshEdge::pressure() const
{
    return pressure_;
}

void Q3MeshEdge::setPressure(const qreal &pressure)
{
    pressure_ = pressure;
}

int Q3MeshEdge::label() const
{
    if (!boundary_)
        return 0;
    return boundary_->label();
}

Q3Boundary *Q3MeshEdge::boundary() const
{
    return boundary_;
}

int Q3MeshEdge::id() const
{
    return id_;
}

qreal Q3MeshEdge::processBoundaryPredictor(qreal Re, bool monotoneTerm, 
                                           QVector2D &tV)
{
    if (!boundary_)
        return 0;

    Q3MeshTriangle *triangle = adjacentTriangles_.at(0);
    int edgeIndex = triangle->edges().indexOf(this);
    qreal dl = triangle->distancesToEdges().at(edgeIndex);

    // TODO: подумать, как перенести в Q3Boundary
    switch (boundary()->type()->toEnum())
    {
        case Q3BoundaryType::NoSlipBoundary:
        {
            return length_ / Re / dl;
        }
        case Q3BoundaryType::InBoundary:
        case Q3BoundaryType::FixedVelocity: // Проверить
        {
            QVector2D normal = triangle->normalVectors().at(edgeIndex);
            qreal vni = QVector2D::dotProduct(velocity_, normal);
            qreal tnu = 0;

            if (monotoneTerm)
                tnu = dl * qAbs(vni) * Re;

            QVector2D deltaV = length_ * (1. / Re / dl * (1. + tnu) - vni) * velocity_;
            tV += deltaV;
            return length_ / Re * (1. + tnu) / dl - length_ * vni;
        }
        case Q3BoundaryType::OutBoundary:
        {
            // Фикс для графиков и корректора
            velocity_ = triangle->correctorVelocity();
            qreal vni = QVector2D::dotProduct(
                            triangle->correctorVelocity(),
                            triangle->normalVectors().at(edgeIndex));
            qreal tnu = 0;
            if (monotoneTerm)
                tnu = dl * qAbs(vni) * Re;

            qreal deltaA = length_ * ((1. + tnu) / Re / dl - vni);
            tV += deltaA * triangle->predictorVelocity();
            return deltaA;
        }
        default:
            break;
    }
    return 0;
}

qreal Q3MeshEdge::processBoundaryFlow()
{
    if (!boundary_)
        return 0;

    Q3MeshTriangle *triangle = adjacentTriangles_.at(0);
    int edgeIndex = triangle->edges().indexOf(this);
    QVector2D normal = triangle->normalVectors().at(edgeIndex);

    // TODO: подумать, как перенести в Q3Boundary
    switch (boundary()->type()->toEnum())
    {
        case Q3BoundaryType::InBoundary:
            return length_ * QVector2D::dotProduct(velocity_, normal);
        case Q3BoundaryType::OutBoundary:
        {
<<<<<<< HEAD
            qreal flow = length_ * QVector2D::dotProduct(triangle->correctorVelocity(),
                                                         normal);
            if (flow < 0)
                flow = 0;
            return flow;
=======
            qreal flow = length_ * QVector2D::dotProduct(velocity_, normal);
            return flow > 0 ? flow : 0;
>>>>>>> master
        }
        default:
            break;
    }

    return 0;
}

qreal Q3MeshEdge::processBoundaryCorrector()
{
    if (!boundary_)
        return 0;

    Q3MeshTriangle *triangle = adjacentTriangles_.at(0);
    int edgeIndex = triangle->edges().indexOf(this);
    QVector2D normal = triangle->normalVectors().at(edgeIndex);
    QVector2D predictorVelocity = triangle->predictorVelocity();

    // TODO: подумать, как перенести в Q3Boundary
    switch (boundary()->type()->toEnum())
    {
        case Q3BoundaryType::InBoundary:
        case Q3BoundaryType::FixedVelocity:
        {
            return QVector2D::dotProduct(velocity_, normal)
                    - QVector2D::dotProduct(predictorVelocity, normal);
        }
        case Q3BoundaryType::NoSlipBoundary:
            return - QVector2D::dotProduct(predictorVelocity, normal);
        default:
            break;
    }

    return 0;
}

void Q3MeshEdge::processBoundaryVorticity(qreal &dvXByY, qreal &dvYByX)
{
    if (!boundary_)
        return;

    Q3MeshTriangle *triangle = adjacentTriangles_.at(0);
    int edgeIndex = triangle->edges().indexOf(this);
    QVector2D normal = triangle->normalVectors().at(edgeIndex);

    switch (boundary()->type()->toEnum())
    {
        case Q3BoundaryType::InBoundary:
        case Q3BoundaryType::FixedVelocity:
        {
            dvXByY += length_ * velocity_.x() * normal.y();
            dvYByX += length_ * velocity_.y() * normal.x();
            return;
        }
        case Q3BoundaryType::OutBoundary:
        {
            dvXByY += length_ * triangle->correctorVelocity().x() * normal.y();
            dvYByX += length_ * triangle->correctorVelocity().y() * normal.x();
            return;
        }
        case Q3BoundaryType::NoSlipBoundary:
            return;
        default:
            break;
    }
}

qreal Q3MeshEdge::processBoundaryStream()
{
    if (!boundary_)
        return 0;

    Q3MeshTriangle *triangle = adjacentTriangles_.at(0);
    int edgeIndex = triangle->edges().indexOf(this);
    QVector2D normal = triangle->normalVectors().at(edgeIndex);

    switch (boundary()->type()->toEnum())
    {
        case Q3BoundaryType::InBoundary:
        case Q3BoundaryType::FixedVelocity:
        {
//            qDebug() << "In" << velocity_;
            return (normal.x() * velocity_.y() - normal.y() * velocity_.x())
                    * length_;
        }
        case Q3BoundaryType::OutBoundary:
        {
//            qDebug() << "Out" << triangle->correctorVelocity();
            return (normal.x() * triangle->correctorVelocity().y()
                    - normal.y() * triangle->correctorVelocity().x())
                    * length_;
        }
        case Q3BoundaryType::NoSlipBoundary:
            return 0;
        default:
            break;
    }

    return 0;
}

void Q3MeshEdge::processBoundaryVelocity(qreal time)
{
    if (!boundary_)
        return;

    if (boundary_->type()->toEnum() == Q3BoundaryType::OutBoundary)
        return;

    velocity_ = boundary_->velocity(*a_, *b_, time);
}


QDataStream &operator<<(QDataStream &stream, const Q3MeshEdge &edge)
{
    stream << edge.a() << " " << edge.b();
}
