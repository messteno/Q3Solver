#include <QTextStream>
#include <QDebug>
#include <QPolygonF>
#include <QPixmap>
#include <QTime>
#include <qmath.h>

#include "q3mesh.h"

Q3Mesh::Q3Mesh(QWidget *parent) :
    Q3PlotDrawable(),
    QWidget(parent),
    square_(0),
    angles_(0)
{

}

Q3Mesh::~Q3Mesh()
{

}

QList<Q3MeshNode *>& Q3Mesh::nodes()
{
    return nodes_;
}

QList<Q3MeshEdge *>& Q3Mesh::edges()
{
    return edges_;
}

QList<Q3MeshTriangle *>& Q3Mesh::triangles()
{
    return triangles_;
}

Q3MeshNode* Q3Mesh::addNode(Q3MeshNode *node)
{
    nodes_.append(node);
    return nodes_.last();
}

Q3MeshNode* Q3Mesh::addNode(QPointF &point)
{
    Q3MeshNode *node = new Q3MeshNode(point);
    nodes_.append(node);
    return node;
}

Q3MeshNode* Q3Mesh::addNode(qreal x, qreal y)
{
    Q3MeshNode *node = new Q3MeshNode(x, y);
    nodes_.append(node);
    return node;
}

Q3MeshEdge* Q3Mesh::addEdge(Q3MeshNode *a, Q3MeshNode *b, Q3Boundary *boundary)
{
    Q3MeshEdge *edge = new Q3MeshEdge(a, b, boundary);
    edges_.append(edge);
    a->addEdge(edge);
    b->addEdge(edge);

    return edge;
}

Q3MeshTriangle* Q3Mesh::addTriangle(Q3MeshEdge *a, Q3MeshEdge *b, Q3MeshEdge *c)
{
    Q3MeshTriangle *triangle = new Q3MeshTriangle(a, b, c);
    triangles_.append(triangle);

    return triangle;
}

void Q3Mesh::draw(Q3Painter &painter) const
{
    drawEdges(painter);
}

void Q3Mesh::drawEdges(Q3Painter &painter) const
{
    qreal scaleX = painter.sx();
    qreal scaleY = painter.sy();

    QListIterator<Q3MeshEdge *> eit(edges_);

    while(eit.hasNext())
    {
        const Q3MeshEdge *edge = eit.next();

        QColor color;
        if (edge->label() != 0)
            color.setHsv((73 * edge->label()) % 360, 230, 180);
        else
            color = QColor(Qt::white);

        painter.setPen(color);

        painter.drawLine(edge->a()->x() * scaleX, edge->a()->y() * scaleY,
                         edge->b()->x() * scaleX, edge->b()->y() * scaleY);
    }
}

void Q3Mesh::drawTriangles(Q3Painter &painter) const
{
    qreal scaleX = painter.sx();
    qreal scaleY = painter.sy();

    QListIterator<Q3MeshTriangle *> tit(triangles_);
    while(tit.hasNext())
    {
        const Q3MeshTriangle *triangle = tit.next();

        if (triangle->isBad())
            painter.setBrush(QColor(163, 39, 39));
        else
            painter.setBrush(QColor(122, 163, 39));

        QPolygonF polygon = triangle->toPolygonF(scaleX, scaleY);
        painter.drawPolygon(polygon);
    }
}

void Q3Mesh::clear()
{
    qDeleteAll(nodes_.begin(), nodes_.end());
    nodes_.clear();
    qDeleteAll(edges_.begin(), edges_.end());
    edges_.clear();
    qDeleteAll(triangles_.begin(), triangles_.end());
    triangles_.clear();

    square_ = 0;
    angles_ = 0;
}

QString Q3Mesh::info()
{
    QString out;
    QTextStream stream(&out);
    stream << tr("Количество узлов: ") << QString::number(nodes_.count()) << "\n";
    stream << tr("Количество ребер: ") << QString::number(edges_.count()) << "\n";
    stream << tr("Количество эл-ов: ") << QString::number(triangles_.count()) << "\n";
    stream << tr("Площадь:          ") << QString::number(square_) << "\n";
    stream << tr("Проверка углов:   ") << QString::number(angles_) << "\n";
    return out;
}

void Q3Mesh::check()
{
    square_ = 0;
    foreach (Q3MeshTriangle *triangle, triangles_)
        square_ += triangle->square();

    angles_ = 0;
    foreach (Q3MeshEdge *edge, edges_)
    {
        foreach (qreal contangent, edge->adjacentCotangents())
        {
            qreal actg = qAtan(contangent);
            actg = M_PI_2 - actg;
            actg = actg * 180. / M_PI;
            angles_ += actg;
        }
    }
    angles_ /= 2. * triangles_.count();
    Q_ASSERT(qAbs(angles_ - 180) < 1e-4);
}
