#include "q3meshnode.h"
#include "q3meshedge.h"

Q3MeshNode::Q3MeshNode(const QPointF &point) :
    QPointF(point)
{

}

Q3MeshNode::Q3MeshNode(qreal x, qreal y) :
    QPointF(x, y)
{

}

QList<Q3MeshEdge *>& Q3MeshNode::edges()
{
    return edges_;
}

void Q3MeshNode::addEdge(Q3MeshEdge *edge)
{
    edges_.append(edge);
}

Q3MeshEdge *Q3MeshNode::edgeAdjacentTo(const Q3MeshNode *node) const
{
    QListIterator<Q3MeshEdge *> eit(edges_);
    while(eit.hasNext())
    {
        Q3MeshEdge *edge = eit.next();
        if (edge->a() == node || edge->b() == node)
            return edge;
    }
    return NULL;
}
