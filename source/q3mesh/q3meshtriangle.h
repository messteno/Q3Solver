#ifndef Q3MESHTRIANGLE_H
#define Q3MESHTRIANGLE_H

#include <QPolygonF>
#include <QList>
#include <QVector2D>

class Q3MeshNode;
class Q3MeshEdge;

class Q3MeshTriangle
{
public:
    Q3MeshTriangle(Q3MeshEdge *a, Q3MeshEdge *b, Q3MeshEdge *c, int id);

    Q3MeshNode *vA() const;
    Q3MeshNode *vB() const;
    Q3MeshNode *vC() const;

    Q3MeshEdge *a() const;
    Q3MeshEdge *b() const;
    Q3MeshEdge *c() const;

    QList<Q3MeshNode *>& vertices();
    QList<Q3MeshEdge *>& edges();
    QVector<Q3MeshTriangle *>& adjacentTriangles();
    QVector<QVector2D>& normalVectors();

    void addAdjacentTriangle(Q3MeshTriangle *triangle, Q3MeshEdge *edge);

    QPolygonF toPolygonF(qreal sx, qreal sy) const;

    qreal square() const;
    QPointF center() const;

    bool isBad() const;

    QVector2D correctorVelocity() const;
    void setCorrectorVelocity(const QVector2D &correctorVelocity);

    QVector2D previousCorrectorVelocity() const;
    void setPreviousCorrectorVelocity(const QVector2D &previousCorrectorVelocity);

    QVector2D predictorVelocity() const;
    void setPredictorVelocity(const QVector2D &predictorVelocity);

    QVector<qreal> distancesToEdges() const;
    void setDistancesToEdges(const QVector<qreal> &distancesToEdges);

    QVector<qreal> distanceToTriangles() const;
    void setDistanceToTriangles(const QVector<qreal> &distanceToTriangles);

    int id() const;

    qreal stream() const;
    void setStream(const qreal &stream);

    qreal vorticity() const;
    void setVorticity(const qreal &vorticity);

    bool hasBoundaryEdge();

private:
    Q3MeshNode *vA_;
    Q3MeshNode *vB_;
    Q3MeshNode *vC_;

    Q3MeshEdge *a_;
    Q3MeshEdge *b_;
    Q3MeshEdge *c_;

    QList<Q3MeshNode *> vertices_;
    QList<Q3MeshEdge *> edges_;

    QVector<Q3MeshTriangle *> adjacentTriangles_;
    QVector<qreal> distanceToTriangles_;
    QVector<qreal> distancesToEdges_;
    QVector<QVector2D> normalVectors_;

    qreal square_;
    QPointF center_;

    QVector2D correctorVelocity_;
    QVector2D previousCorrectorVelocity_;
    QVector2D predictorVelocity_;
    QVector2D tempVelocity_;

    qreal stream_;
    qreal vorticity_;

    bool bad_;
    int id_;
};

#endif // Q3MESHTRIANGLE_H
