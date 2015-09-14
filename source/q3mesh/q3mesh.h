#ifndef Q3MESH_H
#define Q3MESH_H

#include <QPoint>
#include <QList>

#include "q3plotdrawable.h"
#include "q3meshnode.h"
#include "q3meshedge.h"
#include "q3meshtriangle.h"

class Q3Mesh : public Q3PlotDrawable
{
public:

    enum DrawPolicy
    {
        DrawBorders = 1,
        DrawEdges = 2,
        DrawTriangles = 4,
        DrawVelocity = 8
    };

    Q3Mesh();
    ~Q3Mesh();

    QList<Q3MeshNode *>& nodes();
    QList<Q3MeshEdge *>& edges();
    QList<Q3MeshTriangle *>& triangles();

    Q3MeshNode* addNode(QPointF &point);
    Q3MeshNode* addNode(qreal x, qreal y);

    Q3MeshEdge* addEdge(Q3MeshNode *a, Q3MeshNode *b,
                        Q3Boundary *boundary = NULL);

    Q3MeshTriangle* addTriangle(Q3MeshEdge *a, Q3MeshEdge *b, Q3MeshEdge *c);
    void draw(Q3Painter &painter) const;
    void calcStream();
    void calcVorticity();

    // Нужно вызывать после добавления всех элементов
    void update();

    void clear();
    QString info();
    qreal square();

    typedef QList<Q3MeshEdge *> EdgeBoundary;
    typedef QList<EdgeBoundary> EdgeBoundaries;
    EdgeBoundaries& boundaries();

    void setDrawPolicy(const uint &drawPolicy);

    QRectF boundingRect() const;

private:
    QList<Q3MeshNode *> nodes_;			// all points
    QList<Q3MeshEdge *> edges_;			// all edges
    QList<Q3MeshTriangle *> triangles_;	// all triangles

    EdgeBoundaries boundaries_;

    qreal square_;
    qreal edgeSquare_;
    qreal angles_;
    int obtuseTrianglesCount_;

    uint drawPolicy_;
    QRectF boundingRect_;

    static const int maxStreamIterationsCount;
    static const qreal maxStreamError;

    void drawEdges(Q3Painter &painter) const;
    void drawTriangles(Q3Painter &painter) const;
};

QColor getColour(qreal level);

#endif // Q3MESH_H
