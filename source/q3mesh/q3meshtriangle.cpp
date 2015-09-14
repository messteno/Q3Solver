#include <QVector3D>
#include <QDebug>
#include <qmath.h>

#include "q3meshedge.h"
#include "q3meshnode.h"
#include "q3meshtriangle.h"

Q3MeshTriangle::Q3MeshTriangle(Q3MeshEdge *a,
                               Q3MeshEdge *b,
                               Q3MeshEdge *c,
                               int id) :
    a_(a),
    b_(b),
    c_(c),
    id_(id),
    correctorVelocity_(0, 0),
    previousCorrectorVelocity_(0, 0),
    predictorVelocity_(0, 0),
    tempVelocity_(0, 0),
    pressure_(0),
    stream_(0),
    vorticity_(0)
{
    vA_ = b->nodeAdjacentTo(c);
    vB_ = c->nodeAdjacentTo(a);
    vC_ = a->nodeAdjacentTo(b);

    Q_ASSERT(vA_);
    Q_ASSERT(vB_);
    Q_ASSERT(vC_);

    vertices_ << vA_ << vB_ << vC_;
    edges_ << a_ << b_ << c_;

    adjacentTriangles_.fill(NULL, 3);

    square_ = qAbs(QVector3D::crossProduct(QVector3D(*vB_ - *vA_),
                                           QVector3D(*vC_ - *vA_)).length())
              * 0.5;

    // Вычислим центр
    // 1. Если треугольник тупоугольный, то центр масс
    // 2. Иначе центр описанной окружности

    qreal lengthA = a_->length();
    qreal lengthB = b_->length();
    qreal lengthC = c_->length();

    qreal squareLa = lengthA * lengthA;
    qreal squareLb = lengthB * lengthB;
    qreal squareLc = lengthC * lengthC;

    qreal check1 = squareLa + squareLb - squareLc;
    qreal check2 = squareLb + squareLc - squareLa;
    qreal check3 = squareLc + squareLa - squareLb;

    if (check1 <= 0 || check2 <= 0 || check3 <= 0)
    {
        center_ = (*vA_ + *vB_ + *vC_) / 3.;
        bad_ = true;
    }
    else
    {
        qreal x1 = vA_->x();
        qreal y1 = vA_->y();
        qreal x2 = vB_->x();
        qreal y2 = vB_->y();
        qreal x3 = vC_->x();
        qreal y3 = vC_->y();

        qreal b1 = x1 * x1 + y1 * y1;
        qreal b2 = x2 * x2 + y2 * y2;
        qreal b3 = x3 * x3 + y3 * y3;

        qreal a = x1 * y2 + x2 * y3 + x3 * y1 - x3 * y2 - x1 * y3 - x2 * y1;
        qreal bx = (b1 * y2 + b2 * y3 + b3 * y1 - b3 * y2 - b1 * y3 - b2 * y1);
        qreal by = -(b1 * x2 + b2 * x3 + b3 * x1 - b3 * x2 - b1 * x3 - b2 * x1);

        center_ = QPointF(0.5 * bx / a, 0.5 * by / a);
        bad_ = false;
    }

    distancesToEdges_ << QVector2D(center_ - a_->center()).length()
                      << QVector2D(center_ - b_->center()).length()
                      << QVector2D(center_ - c_->center()).length();
    distanceToTriangles_ = distancesToEdges_;

    int vertexIndex = 0;
    foreach (Q3MeshEdge* const& edge, edges_)
    {
        QVector2D normalVector = edge->normalVector();

        // Проверим нормали на в(шив/неш)ость =)
        QPointF oppositePoint = *vertices_.at(vertexIndex);
        qreal cross1 = QVector3D::crossProduct(
                           QVector3D(*edge->b() - *edge->a()),
                           QVector3D(oppositePoint - *edge->a())).z();
        qreal cross2 = QVector3D::crossProduct(
                           QVector3D(*edge->b() - *edge->a()),
                           QVector3D(normalVector)).z();
        if (cross1 * cross2 > 0)
            normalVector *= -1;

        normalVectors_ << normalVector;

        ++vertexIndex;
    }

    foreach (Q3MeshEdge* const &edge, edges_)
        edge->addAdjacentTriangle(this);
}

Q3MeshNode *Q3MeshTriangle::vA() const
{
    return vA_;
}

Q3MeshNode *Q3MeshTriangle::vB() const
{
    return vB_;
}

Q3MeshNode *Q3MeshTriangle::vC() const
{
    return vC_;
}

Q3MeshEdge *Q3MeshTriangle::a() const
{
    return a_;
}

Q3MeshEdge *Q3MeshTriangle::b() const
{
    return b_;
}

Q3MeshEdge *Q3MeshTriangle::c() const
{
    return c_;
}

QList<Q3MeshNode *> &Q3MeshTriangle::vertices()
{
    return vertices_;
}

QList<Q3MeshEdge *> &Q3MeshTriangle::edges()
{
    return edges_;
}

QVector<Q3MeshTriangle *> &Q3MeshTriangle::adjacentTriangles()
{
    return adjacentTriangles_;
}

QVector<QVector2D> &Q3MeshTriangle::normalVectors()
{
    return normalVectors_;
}

void Q3MeshTriangle::addAdjacentTriangle(Q3MeshTriangle *triangle,
                                         Q3MeshEdge *edge)
{
    Q_ASSERT(triangle != this);
    Q_ASSERT(!adjacentTriangles_.contains(triangle));
    Q_ASSERT(edges_.contains(edge));

    int index = edges_.indexOf(edge);
    Q_ASSERT(index >= 0 && index < 3);

    adjacentTriangles_[index] = triangle;
    distanceToTriangles_[index] =
            QVector2D(center_ - triangle->center()).length();
    distancesToEdges_[index] = QVector2D(center_ - edge->center()).length();
}

QPolygonF Q3MeshTriangle::toPolygonF(qreal sx, qreal sy) const
{
    QPolygonF polygon;
    polygon << QPointF(vA_->x() * sx, vA_->y() * sy);
    polygon << QPointF(vB_->x() * sx, vB_->y() * sy);
    polygon << QPointF(vC_->x() * sx, vC_->y() * sy);

    return polygon;
}

qreal Q3MeshTriangle::square() const
{
    return square_;
}

QPointF Q3MeshTriangle::center() const
{
    return center_;
}

bool Q3MeshTriangle::isBad() const
{
    return bad_;
}

QVector2D Q3MeshTriangle::correctorVelocity() const
{
    return correctorVelocity_;
}

void Q3MeshTriangle::setCorrectorVelocity(const QVector2D &correctorVelocity)
{
    correctorVelocity_ = correctorVelocity;
}

QVector2D Q3MeshTriangle::previousCorrectorVelocity() const
{
    return previousCorrectorVelocity_;
}

void Q3MeshTriangle::setPreviousCorrectorVelocity(const QVector2D &previousCorrectorVelocity)
{
    previousCorrectorVelocity_ = previousCorrectorVelocity;
}

QVector2D Q3MeshTriangle::predictorVelocity() const
{
    return predictorVelocity_;
}

void Q3MeshTriangle::setPredictorVelocity(const QVector2D &predictorVelocity)
{
    predictorVelocity_ = predictorVelocity;
}

QVector<qreal> Q3MeshTriangle::distancesToEdges() const
{
    return distancesToEdges_;
}

QVector<qreal> Q3MeshTriangle::distanceToTriangles() const
{
    return distanceToTriangles_;
}

int Q3MeshTriangle::id() const
{
    return id_;
}

qreal Q3MeshTriangle::stream() const
{
    return stream_;
}

void Q3MeshTriangle::setStream(const qreal &stream)
{
    stream_ = stream;
}

qreal Q3MeshTriangle::vorticity() const
{
    return vorticity_;
}

void Q3MeshTriangle::setVorticity(const qreal &omega)
{
    vorticity_ = omega;
}

bool Q3MeshTriangle::hasBoundaryEdge()
{
    return a_->boundary() || b_->boundary() || c_->boundary();
}

qreal Q3MeshTriangle::pressure() const
{
    return pressure_;
}

void Q3MeshTriangle::setPressure(const qreal &pressure)
{
    pressure_ = pressure;
}

qreal Q3MeshTriangle::divergence(bool predictor) const
{
    qreal delta = 0.;
    QVector2D velocity = predictor ? predictorVelocity_ : correctorVelocity_;

    for (int edInd = 0; edInd < edges_.count(); ++edInd)
    {
        Q3MeshEdge *edge = edges_.at(edInd);
        Q3MeshTriangle *adjacentTriangle = adjacentTriangles_.at(edInd);
        QVector2D normal = normalVectors_.at(edInd);
        qreal dl = distancesToEdges_.at(edInd);

        if (adjacentTriangle)
        {
            QVector2D adjVelocity = predictor
                                    ? adjacentTriangle->predictorVelocity_
                                    : adjacentTriangle->correctorVelocity_;
            qreal dL = distanceToTriangles_.at(edInd);
            qreal vni = (dl * QVector2D::dotProduct(adjVelocity, normal)
                         + (dL - dl) * QVector2D::dotProduct(velocity, normal)) / dL;
            delta += vni * edge->length();
        }
        else
        {
            qreal vni = 0;
            switch (edge->boundary()->type()->toEnum())
            {
                case Q3BoundaryType::FixedVelocity:
                case Q3BoundaryType::InBoundary:
                    vni = QVector2D::dotProduct(edge->velocity(), normal);
                    break;
                case Q3BoundaryType::OutBoundary:
                    vni = QVector2D::dotProduct(velocity, normal);
                    break;
                default:
                    break;
            }
            if (edge->boundary()->type()->toEnum() == Q3BoundaryType::InBoundary)
                qDebug() << edge->velocity();
            delta += vni * edge->length();
        }
    }

    delta /= square_;
    return delta;
}
