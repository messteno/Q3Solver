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
    Q3MeshTriangle(Q3MeshEdge *a, Q3MeshEdge *b, Q3MeshEdge *c);

    Q3MeshNode *vA() const;
    Q3MeshNode *vB() const;
    Q3MeshNode *vC() const;

    Q3MeshEdge *a() const;
    Q3MeshEdge *b() const;
    Q3MeshEdge *c() const;

    QList<Q3MeshNode *> vertices() const;
    QList<Q3MeshEdge *> edges() const;
    QVector<Q3MeshTriangle *> adjacentTriangles() const;
    QVector<QVector2D> normalVectors() const;

    void addAdjacentTriangle(Q3MeshTriangle *triangle, Q3MeshEdge *edge);

    QPolygonF toPolygonF(qreal sx, qreal sy) const;

    qreal square() const;
    QPointF center() const;

    bool isBad() const;

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

    bool bad_;
};

#endif // Q3MESHTRIANGLE_H
