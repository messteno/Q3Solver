#ifndef Q3MESHNODE_H
#define Q3MESHNODE_H

#include <QList>
#include <QPointF>
#include <QVector2D>

class Q3MeshEdge;

class Q3MeshNode : public QPointF
{
public:
    Q3MeshNode(const QPointF &point, int id);

    QList<Q3MeshEdge *>& edges();
    void addEdge(Q3MeshEdge *edge);

    Q3MeshEdge *edgeAdjacentTo(const Q3MeshNode *node) const;

    int id() const;

    bool boundary() const;
    void setBoundary(bool boundary);

private:
    QList<Q3MeshEdge *> edges_;
    int id_;
    bool boundary_;
};


#endif // Q3MESHNODE_H
