#ifndef Q3MESH_H
#define Q3MESH_H

#include <QPoint>
#include <QVector>

#include "q3plotdrawable.h"

class Q3MeshEdge;

class Q3MeshNode : public QPointF
{
public:
    Q3MeshNode();
    Q3MeshNode(const QPointF &point);
    Q3MeshNode(qreal x, qreal y);

    QList<Q3MeshEdge *>& edges();
    void addEdge(Q3MeshEdge *edge);

    bool adjacentTo(const Q3MeshNode *node) const;

private:
    QList<Q3MeshEdge *> edges_;
};

class Q3MeshEdge
{
public:
    Q3MeshEdge();
    Q3MeshEdge(Q3MeshNode *a, Q3MeshNode *b);

    Q3MeshNode *a() const;
    Q3MeshNode *b() const;

    bool adjacentTo(Q3MeshEdge *edge);

private:
    Q3MeshNode *a_;
    Q3MeshNode *b_;
};

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

private:
    Q3MeshNode *vA_;
    Q3MeshNode *vB_;
    Q3MeshNode *vC_;

    Q3MeshEdge *a_;
    Q3MeshEdge *b_;
    Q3MeshEdge *c_;
};

class Q3Mesh : public QWidget, public Q3PlotDrawable
{
    Q_OBJECT
public:
    enum BoundaryType
    {
        CannotBeBoundary,
        NotBoundary,
        InBoundary,
        OutBoundary,
        MoveBoundary,
    };

    Q3Mesh(QWidget *parent);
    ~Q3Mesh();

    QVector<Q3MeshNode>& nodes();
    QVector<Q3MeshEdge>& edges();
    QVector<Q3MeshTriangle>& triangles();

    void addNode(const Q3MeshNode &node);
    void addNode(const QPointF &point);
    void addNode(qreal x, qreal y);

    void addEdge(Q3MeshEdge &edge);
    void addEdge(Q3MeshNode *a, Q3MeshNode *b);

    void addTriangle(Q3MeshTriangle &triangle);
    void addTriangle(Q3MeshEdge *a, Q3MeshEdge *b, Q3MeshEdge *c);

    void draw(Q3Painter &painter) const;

    void clear();

    static QString boundaryTypeToString(const BoundaryType &type);
private:
    QVector<Q3MeshNode> nodes_;			// all points
    QVector<Q3MeshEdge> edges_;			// all edges
    QVector<Q3MeshTriangle> triangles_;	// all triangles
};

#endif // Q3MESH_H
