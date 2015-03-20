#include "q3mesh.h"

Q3MeshNode::Q3MeshNode() :
    QPointF()
{

}

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

bool Q3MeshNode::adjacentTo(const Q3MeshNode *node) const
{
    QListIterator<Q3MeshEdge *> eit(edges_);
    while(eit.hasNext())
    {
        Q3MeshEdge *edge = eit.next();
        if (edge->a() == node || edge->b() == node)
            return true;
    }
    return false;
}

Q3MeshEdge::Q3MeshEdge()
{

}

Q3MeshEdge::Q3MeshEdge(Q3MeshNode *a, Q3MeshNode *b) :
    a_(a),
    b_(b)
{
}

Q3MeshNode *Q3MeshEdge::a() const
{
    return a_;
}

Q3MeshNode *Q3MeshEdge::b() const
{
    return b_;
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

Q3Mesh::Q3Mesh(QWidget *parent) :
    Q3PlotDrawable(parent)
{

}

Q3Mesh::~Q3Mesh()
{

}

QVector<Q3MeshNode>& Q3Mesh::nodes()
{
    return nodes_;
}
QVector<Q3MeshEdge>& Q3Mesh::edges()
{
    return edges_;
}

QVector<Q3MeshTriangle>& Q3Mesh::triangles()
{
    return triangles_;
}

void Q3Mesh::addNode(const Q3MeshNode &node)
{
    nodes_.append(node);
}

void Q3Mesh::addNode(const QPointF &point)
{
    nodes_.append(Q3MeshNode(point));
}

void Q3Mesh::addNode(qreal x, qreal y)
{
    nodes_.append(Q3MeshNode(x, y));
}

void Q3Mesh::addEdge(Q3MeshNode *a, Q3MeshNode *b)
{
    Q3MeshEdge edge(a, b);
    edges_.append(edge);
    a->addEdge(&edges_.last());
    b->addEdge(&edges_.last());
}

void Q3Mesh::draw(Q3Painter &painter) const
{
    qreal scaleX = painter.sx();
    qreal scaleY = painter.sy();
    QVectorIterator<Q3MeshEdge> eit(edges_);

    while(eit.hasNext())
    {
        const Q3MeshEdge &edge = eit.next();
        painter.drawLine(edge.a()->x() * scaleX, edge.a()->y() * scaleY,
                         edge.b()->x() * scaleX, edge.b()->y() * scaleY);
    }
}

void Q3Mesh::clear()
{
    nodes_.clear();
    edges_.clear();
    triangles_.clear();
}
