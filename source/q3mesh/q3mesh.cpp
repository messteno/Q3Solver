#include <QTextStream>
#include <QDebug>
#include <QPolygonF>
#include <QFile>
#include <QPixmap>
#include <QTime>
#include <qmath.h>

#include "q3mesh.h"
#include "preconditioner.h"
#include "q3meshlaplacestreamoperator.h"
#include "bicgstablinearsolver.h"

const int Q3Mesh::maxStreamIterationsCount = 5000;
const qreal Q3Mesh::maxStreamError = 1e-8;

Q3Mesh::Q3Mesh() :
    Q3PlotDrawable(),
    square_(0),
    edgeSquare_(0),
    angles_(0),
    drawPolicy_(DrawTriangles),
    obtuseTrianglesCount_(0)
{

}

Q3Mesh::~Q3Mesh()
{
    clear();
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

Q3MeshNode* Q3Mesh::addNode(QPointF &point)
{
    Q3MeshNode *node = new Q3MeshNode(point, nodes_.count());
    nodes_.append(node);
    return node;
}

Q3MeshNode* Q3Mesh::addNode(qreal x, qreal y)
{
    Q3MeshNode *node = new Q3MeshNode(QPointF(x, y), nodes_.count());
    nodes_.append(node);
    return node;
}

Q3MeshEdge* Q3Mesh::addEdge(Q3MeshNode *a, Q3MeshNode *b, Q3Boundary *boundary)
{
    Q3MeshEdge *edge = a->edgeAdjacentTo(b);
    if (edge)
        return edge;

    // id доолжен совпадать с индексом в списке
    edge = new Q3MeshEdge(a, b, boundary, edges_.count());
    edges_.append(edge);
    a->addEdge(edge);
    b->addEdge(edge);

    return edge;
}

Q3MeshTriangle* Q3Mesh::addTriangle(Q3MeshEdge *a, Q3MeshEdge *b, Q3MeshEdge *c)
{
    Q3MeshTriangle *triangle = new Q3MeshTriangle(a, b, c, triangles_.count());
    triangles_.append(triangle);

    return triangle;
}

void Q3Mesh::draw(Q3Painter &painter) const
{
    if (drawPolicy_ & DrawEdges || drawPolicy_ & DrawBorders)
        drawEdges(painter);

    if (drawPolicy_ & DrawTriangles)
        drawTriangles(painter);

    qreal scaleX = painter.sx();
    qreal scaleY = painter.sy();

    if (drawPolicy_ & DrawVelocity)
    {
        painter.setPen(QColor(122, 163, 39));
        for (int trIndex = 0; trIndex < triangles_.count(); ++trIndex)
        {
            Q3MeshTriangle *triangle = triangles_.at(trIndex);
            QPointF begin = triangle->center();
            begin = QPointF(begin.x() * scaleX, begin.y() * scaleY);
            QPointF end = (QVector2D(triangle->center())
                           + 0.1 * triangle->predictorVelocity()).toPointF();
            end = QPointF(end.x() * scaleX, end.y() * scaleY);
            painter.drawLine(begin, end);
//            painter.drawText(triangle->center().x() * scaleX,
//                             triangle->center().y() * scaleY,
//                             QString::number(triangle->stream(), 'd', 6));
        }

        for (int eInd = 0; eInd < edges_.count(); ++eInd)
        {
            Q3MeshEdge *edge = edges_.at(eInd);
            if (!edge->boundary())
                continue;
            QPointF begin = edge->center();
            begin = QPointF(begin.x() * scaleX, begin.y() * scaleY);
            QPointF end = (QVector2D(edge->center())
                           + 0.1 * edge->velocity()).toPointF();
            end = QPointF(end.x() * scaleX, end.y() * scaleY);
            painter.setPen(QColor(22, 63, 188));
            painter.drawLine(begin, end);
//            painter.setPen(Qt::red);
//            painter.drawPoint(begin);
//            painter.drawText(triangle->center().x() * scaleX,
//                             triangle->center().y() * scaleY,
//                             QString::number(triangle->stream(), 'd', 6));
        }

//        for (int nodeIndex = 0; nodeIndex < nodeValues_.count(); ++nodeIndex)
//        {
//            Q3MeshNode *node = nodes_.at(nodeIndex);
//            painter.drawText(node->x() * scaleX,
//                             node->y() * scaleY,
//                             QString::number(nodeValues_.at(nodeIndex), 'd', 6));
//        }
    }
}

void Q3Mesh::drawEdges(Q3Painter &painter) const
{
    qreal scaleX = painter.sx();
    qreal scaleY = painter.sy();

    QListIterator<Q3MeshEdge *> eit(edges_);

    while(eit.hasNext())
    {
        const Q3MeshEdge *edge = eit.next();
        if (!(drawPolicy_ & DrawEdges) && !edge->boundary())
            continue;

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

    // Возможно переделать на count циклы
    QListIterator<Q3MeshTriangle *> tit(triangles_);
    while(tit.hasNext())
    {
        const Q3MeshTriangle *triangle = tit.next();

//        if (triangle->isBad())
//            painter.setBrush(QColor(163, 39, 39));
//        else
//            painter.setBrush(QColor(122, 163, 39));

        QPolygonF polygon = triangle->toPolygonF(scaleX, scaleY);
        painter.drawPolygon(polygon);
    }
}

void Q3Mesh::calcStream()
{
    calcVorticity();

    Q3Vector vorticity(triangles_.size());
    Q3Vector stream(triangles_.size());

//    for (int i = 0; i < triangles_.count(); ++i)
//    {
//        bool noslip = false;
//        foreach (Q3MeshEdge *edge, triangles_.at(i)->edges())
//        {
//            // TODO: добавить как условие на границу
//            if (edge->boundary()
//                && (edge->boundary()->type()->toEnum() == Q3BoundaryType::FixedVelocity
//                    || edge->boundary()->type()->toEnum() == Q3BoundaryType::NoSlipBoundary))
//            {
//                noslip = true;
//                break;
//            }
//        }
//        if (!noslip)
//            vorticity[i] = -triangles_.at(i)->vorticity();
//        else
//            vorticity[i] = 0;

//        stream[i] = triangles_.at(i)->stream();
////        else
////            stream[i] = 0;
//    }

//    IdentityPreconditioner preconditioner(triangles_.count());
//    Q3MeshLaplaceStreamOperator laplaceStreamOperator(*this, triangles_.count());

//    BiCGStabLinearSolver solver(1e-8, 1000);
//    qreal residual = 0;
//    int it = solver.solve(laplaceStreamOperator,
//                          stream,
//                          vorticity,
//                          preconditioner,
//                          residual);

//    for (int i = 0; i < triangles_.count(); ++i)
//        triangles_.at(i)->setStream(stream[i]);

    for (int i = 0; i < triangles_.count(); ++i)
    {
        vorticity[i] = triangles_.at(i)->vorticity();
        stream[i] = triangles_.at(i)->stream();
    }

    int it = 0;
    qreal residual = 0;

    while (it < maxStreamIterationsCount)
    {
        residual = 0;
        for (int trInd = 0; trInd < triangles_.count(); ++trInd)
        {
            Q3MeshTriangle *triangle = triangles_.at(trInd);
            qreal A = 0;
            qreal B = 0;

            for (int eInd = 0; eInd < triangle->edges().size(); ++eInd)
            {
                Q3MeshEdge *edge = triangle->edges().at(eInd);
                Q3MeshTriangle *adjTriangle = triangle->adjacentTriangles().at(eInd);
                QVector2D normal = triangle->normalVectors().at(eInd);

                if (adjTriangle)
                {
                    qreal dL = triangle->distanceToTriangles().at(eInd);

                    QVector2D tAt(adjTriangle->center() - triangle->center());
                    tAt.normalize();

                    qreal cosin = qAbs(QVector2D::dotProduct(tAt, normal));
                    A += edge->length() / dL / cosin;
                    B += edge->length() / dL * adjTriangle->stream() / cosin;
                }
                else
                {
                    qreal deltaB = edge->processBoundaryStream();
                    B += deltaB;
                }
            }
            qreal diff = (vorticity[trInd] + B) / A - stream[trInd];
            stream[trInd] += diff;

            qreal trStreamDelta = qAbs(diff);
            if (trStreamDelta > residual)
                residual = trStreamDelta;
        }

        for (int trInd = 0; trInd < triangles_.count(); ++trInd)
        {
            Q3MeshTriangle *triangle = triangles_.at(trInd);
            triangle->setStream(stream[trInd]);
        }

        if (residual < maxStreamError)
            break;
        it++;
    }

    qDebug() << "Stream: it = " << it << ", res = " << residual;
}

void Q3Mesh::calcVorticity()
{
    for (int trInd = 0; trInd < triangles_.count(); ++trInd)
    {
        Q3MeshTriangle *triangle = triangles_.at(trInd);
        qreal dvXByY = 0;
        qreal dvYByX = 0;

        for (int eInd = 0; eInd < triangle->edges().count(); ++eInd)
        {
            Q3MeshEdge *edge = triangle->edges().at(eInd);
            Q3MeshTriangle *adjTriangle = triangle->adjacentTriangles().at(eInd);
            if (adjTriangle)
            {
                qreal dL = triangle->distanceToTriangles().at(eInd);
                qreal dl = triangle->distancesToEdges().at(eInd);
                QVector2D vv = (triangle->correctorVelocity() * (dL - dl)
                                + adjTriangle->correctorVelocity() * dl) / dL;
                dvXByY += edge->length() * vv.x()
                          * triangle->normalVectors().at(eInd).y();
                dvYByX += edge->length() * vv.y()
                          * triangle->normalVectors().at(eInd).x();
            }
            else
            {
                edge->processBoundaryVorticity(dvXByY, dvYByX);
            }
        }
        triangle->setVorticity((dvXByY - dvYByX) / triangle->square());
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

    boundaries_.clear();

    square_ = 0;
    angles_ = 0;
    edgeSquare_ = 0;
    obtuseTrianglesCount_ = 0;
}

QString Q3Mesh::info()
{
    QString out;
    QTextStream stream(&out);
    stream << QObject::tr("Количество узлов:  ")
           << QString::number(nodes_.count()) << "\n";
    stream << QObject::tr("Количество ребер:  ")
           << QString::number(edges_.count()) << "\n";
    stream << QObject::tr("Количество тр-в:   ")
           << QString::number(triangles_.count()) << "\n";
    stream << QObject::tr("Площадь:           ")
           << QString::number(square_) << "\n";
    stream << QObject::tr("Площадь (ребра):   ")
           << QString::number(edgeSquare_) << "\n";
    stream << QObject::tr("Кол-во границ:     ")
           << QString::number(boundaries_.count()) << "\n";
    stream << QObject::tr("Проверка углов:    ")
           << QString::number(angles_) << "\n";
    stream << QObject::tr("Тупоугольных тр-в: ")
           << QString::number((100. * obtuseTrianglesCount_) / triangles_.count())
           << "\n";
    return out;
}

qreal Q3Mesh::square()
{
    return square_;
}

Q3Mesh::EdgeBoundaries &Q3Mesh::boundaries()
{
    return boundaries_;
}

void Q3Mesh::setDrawPolicy(const uint &drawPolicy)
{
    drawPolicy_ = drawPolicy;
}

QRectF Q3Mesh::boundingRect() const
{
    return boundingRect_;
}

void Q3Mesh::update()
{
    square_ = 0;
    obtuseTrianglesCount_ = 0;
    foreach (Q3MeshTriangle *triangle, triangles_)
    {
        square_ += triangle->square();
        if (triangle->isBad())
            obtuseTrianglesCount_++;
    }

    angles_ = 0;
    edgeSquare_ = 0;
    foreach (Q3MeshEdge *edge, edges_)
    {
        foreach (qreal contangent, edge->adjacentCotangents())
        {
            qreal actg = qAtan(contangent);
            actg = M_PI_2 - actg;
            actg = actg * 180. / M_PI;
            angles_ += actg;
        }

        edgeSquare_ += edge->adjacentSquare();
    }
    angles_ /= 2. * triangles_.count();
    Q_ASSERT(qAbs(angles_ - 180) < 1e-4);

    boundingRect_ = QRectF(*nodes_[0], *nodes_[0]);
    foreach (Q3MeshNode *node, nodes_)
    {
        if (node->x() < boundingRect_.left())
            boundingRect_.setLeft(node->x());
        else if (node->x() > boundingRect_.right())
            boundingRect_.setRight(node->x());

        if (node->y() > boundingRect_.bottom())
            boundingRect_.setBottom(node->y());
        else if (node->y() < boundingRect_.top())
            boundingRect_.setTop(node->y());
    }
}
