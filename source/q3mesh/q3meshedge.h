#ifndef Q3MESHEDGE_H
#define Q3MESHEDGE_H

#include <QList>
#include <QPointF>
#include <QVector2D>

class Q3MeshNode;
class Q3MeshTriangle;

class Q3MeshEdge
{
public:
    Q3MeshEdge(Q3MeshNode *a, Q3MeshNode *b);

    Q3MeshNode *a() const;
    Q3MeshNode *b() const;

    Q3MeshNode *nodeAdjacentTo(Q3MeshEdge *edge);

    QPointF center() const;
    qreal length() const;

    void addAdjacentTriangle(Q3MeshTriangle *triangle);
    void addAdjacentEdge(Q3MeshEdge *edge);

    QList<Q3MeshNode *> vertices() const;
    QList<qreal> adjacentCotangents() const;
    QList<Q3MeshEdge *> adjacentEdges() const;
    QList<Q3MeshTriangle *> adjacentTriangles() const;

    QPointF cross(const QPointF &p1, const QPointF &p2);
    QVector2D normalVector();

private:
    Q3MeshNode *a_;
    Q3MeshNode *b_;

    QList<Q3MeshEdge *> adjacentEdges_;
    QList<Q3MeshTriangle *> adjacentTriangles_;
    QList<qreal> adjacentCotangents_;

    QPointF center_;
    qreal length_;

    QList<Q3MeshNode *> vertices_;
};

#endif // Q3MESHEDGE_H