#ifndef Q3MESH_H
#define Q3MESH_H

#include <QPoint>
#include <QList>

#include "q3plotdrawable.h"
#include "q3meshnode.h"
#include "q3meshedge.h"
#include "q3meshtriangle.h"

class Q3Mesh : public QWidget, public Q3PlotDrawable
{
    Q_OBJECT
public:
    Q3Mesh(QWidget *parent);
    ~Q3Mesh();

    QList<Q3MeshNode *>& nodes();
    QList<Q3MeshEdge *>& edges();
    QList<Q3MeshTriangle *>& triangles();

    Q3MeshNode* addNode(Q3MeshNode *node);
    Q3MeshNode* addNode(QPointF &point);
    Q3MeshNode* addNode(qreal x, qreal y);

    Q3MeshEdge* addEdge(Q3MeshNode *a, Q3MeshNode *b);

    Q3MeshTriangle* addTriangle(Q3MeshEdge *a, Q3MeshEdge *b, Q3MeshEdge *c);

    void draw(Q3Painter &painter) const;

    // Нужно вызывать после добавления всех элементов
    void check();

    void clear();

    QString info();
private:
    QList<Q3MeshNode *> nodes_;			// all points
    QList<Q3MeshEdge *> edges_;			// all edges
    QList<Q3MeshTriangle *> triangles_;	// all triangles

    qreal square_;
    qreal angles_;

    void drawEdges(Q3Painter &painter) const;
    void drawTriangles(Q3Painter &painter) const;
};

#endif // Q3MESH_H