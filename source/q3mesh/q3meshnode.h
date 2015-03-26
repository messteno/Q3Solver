#ifndef Q3MESHNODE_H
#define Q3MESHNODE_H

#include <QList>
#include <QPointF>

class Q3MeshEdge;

class Q3MeshNode : public QPointF
{
public:
    Q3MeshNode(const QPointF &point);
    Q3MeshNode(qreal x, qreal y);

    QList<Q3MeshEdge *>& edges();
    void addEdge(Q3MeshEdge *edge);

    Q3MeshEdge *edgeAdjacentTo(const Q3MeshNode *node) const;

private:
    QList<Q3MeshEdge *> edges_;
};


#endif // Q3MESHNODE_H
