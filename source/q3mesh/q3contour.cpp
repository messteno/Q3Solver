#include <QDebug>

#include "q3contour.h"

Q3ContourGenerator::Q3ContourGenerator(Q3Mesh *mesh,
                                       const QVector<qreal> &values) :
    mesh_(mesh),
    values_(values),
    interiorVisited_(mesh->triangles().count()),
    boundariesVisited_(0),
    boundariesUsed_(0)
{
    // TODO: исправить на какую-то проверку
    Q_ASSERT(mesh_->nodes().size() == values_.size());
}

Q3ContourGenerator::~Q3ContourGenerator()
{

}

Q3Contour Q3ContourGenerator::createContour(qreal level)
{
    Q3Contour contour;

    clearVisitedFlags(false);
    findBoundaryLines(contour, level);
    findInteriorLines(contour, level, false, false);

    return contour;
}

void Q3ContourGenerator::clearVisitedFlags(bool includeBoundaries)
{
    qFill(interiorVisited_.begin(), interiorVisited_.end(), false);

    if (!includeBoundaries)
        return;

    if (boundariesVisited_.isEmpty())
    {
        boundariesVisited_.reserve(mesh_->boundaries().size());
        foreach (const Q3Mesh::EdgeBoundary &boundary, mesh_->boundaries())
            boundariesVisited_.append(QVector<bool>(boundary.size()));

        boundariesUsed_ = QVector<bool>(mesh_->boundaries().size());
    }

    for (QVector<QVector<bool> >::iterator it = boundariesVisited_.begin();
         it != boundariesVisited_.end(); ++it)
        qFill(*it, false);

    qFill(boundariesUsed_, false);
}

void Q3ContourGenerator::findBoundaryLines(Q3Contour &contour, qreal level)
{
    Q3Mesh::EdgeBoundaries &boundaries = mesh_->boundaries();
    for (Q3Mesh::EdgeBoundaries::iterator it = boundaries.begin();
         it != boundaries.end(); ++it)
    {
        Q3Mesh::EdgeBoundary &boundary = *it;
        bool startAbove, endAbove = false;
        for (Q3Mesh::EdgeBoundary::iterator itb = boundary.begin();
             itb != boundary.end(); ++itb)
        {
            Q3MeshEdge* edge = *itb;
            Q3MeshTriangle* triangle = edge->adjacentTriangles().at(0);

            if (itb == boundary.begin())
                startAbove = values_.at(edge->a()->id()) >= level;
            else
                startAbove = endAbove;

            endAbove = values_.at(edge->b()->id()) >= level;
            if (startAbove && !endAbove)
            {
                contour.append(Q3ContourLine());
                Q3ContourLine &contourLine = contour.last();
                followInterior(contourLine, triangle,
                               edge, true, level, false);
            }
        }
    }
}

void Q3ContourGenerator::findInteriorLines(Q3Contour &contour, qreal level,
                                           bool onUpper, bool filled)
{
    int trCount = mesh_->triangles().count();
    for (int trIndex = 0; trIndex < trCount; ++trIndex)
    {
        int visitedIndex = (onUpper ? trIndex + trCount : trIndex);

        if (interiorVisited_[visitedIndex])
            continue;

        interiorVisited_[visitedIndex] = true;
        Q3MeshTriangle *triangle = mesh_->triangles().at(trIndex);
        Q3MeshEdge *edge = getExitEdge(triangle, level, onUpper);
        if (!edge)
            continue;

        QList<Q3MeshTriangle *> &adjTri = edge->adjacentTriangles();
        if (adjTri.count() == 1)
            continue;

        contour.append(Q3ContourLine());
        Q3ContourLine &contourLine = contour.last();

        triangle = adjTri.at(0) == triangle ? adjTri.at(1) : adjTri.at(0);
        followInterior(contourLine, triangle, edge, false, level, onUpper);

        if (!filled)
            contourLine.append(contourLine.first());
        else if (contourLine.count() > 1 &&
                 contourLine.first() == contourLine.last())
        {
            contourLine.removeLast();
        }
    }
}

void Q3ContourGenerator::followInterior(Q3ContourLine &contourLine,
                                        Q3MeshTriangle* &triangle,
                                        Q3MeshEdge* &edge,
                                        bool endOnBoundary,
                                        qreal level,
                                        bool onUpper)
{
    Q_ASSERT(triangle->edges().contains(edge));

    contourLine.append(edgeInterpolation(edge, level));
    while (true)
    {
        int visitedIndex = triangle->id();
        if (onUpper)
            visitedIndex += mesh_->triangles().count();

        if (!endOnBoundary && interiorVisited_[visitedIndex])
            break;

        edge = getExitEdge(triangle, level, onUpper);
        Q_ASSERT(edge);

        interiorVisited_[visitedIndex] = true;
        
        contourLine.append(edgeInterpolation(edge, level));

        if (endOnBoundary && edge->boundary())
            break;

        QList<Q3MeshTriangle *> &adjTri = edge->adjacentTriangles();
        Q_ASSERT(adjTri.count() == 2);
        triangle = adjTri.at(0) == triangle ? adjTri.at(1) : adjTri.at(0);
    }
}

QPointF Q3ContourGenerator::edgeInterpolation(const Q3MeshEdge *edge,
                                              qreal level)
{
    QPointF p1 = *edge->a();
    QPointF p2 = *edge->b();
    qreal v1 = values_.at(edge->a()->id());
    qreal v2 = values_.at(edge->b()->id());
    if (qAbs(v2 - v1) < 1e-15)
        return 0.5 * (p1 + p2);

    qreal fraction = (v2 - level) / (v2 - v1);
    return fraction * p1 + (1. - fraction) * p2;
}

Q3MeshEdge *Q3ContourGenerator::getExitEdge(Q3MeshTriangle *triangle,
                                            qreal level, bool onUpper)
{
    unsigned int config =
            (values_[triangle->vA()->id()] >= level) |
            (values_[triangle->vB()->id()] >= level) << 1 |
            (values_[triangle->vC()->id()] >= level) << 2;

    if (onUpper)
        config = 7 - config;

    switch (config)
    {
        case 0:
            return NULL;
        case 1:
            return triangle->b();
        case 2:
            return triangle->c();
        case 3:
            return triangle->b();
        case 4:
            return triangle->a();
        case 5:
            return triangle->a();
        case 6:
            return triangle->c();
        case 7:
            return NULL;
        default:
            return NULL;
    }
    return NULL;
}

Q3ContourLine::Q3ContourLine()
{

}

Q3Contour::Q3Contour() :
    QList<Q3ContourLine>()
{

}

void Q3Contour::draw(Q3Painter &painter) const
{
    qreal scaleX = painter.sx();
    qreal scaleY = painter.sy();

    for (int i = 0; i < this->size(); ++i)
    {
        const Q3ContourLine &line = this->at(i);
        if (line.empty())
            continue;

        QPointF p = line.at(0);
        QPainterPath path(QPointF(p.x() * scaleX, p.y() * scaleY));
        for (int j = 1; j < line.size(); ++j)
        {
            p = line.at(j);
            path.lineTo(p.x() * scaleX, p.y() * scaleY);
        }
        painter.setPen(Qt::gray);
        painter.setBrush(Qt::transparent);
        painter.drawPath(path);
    }
}


Q3ContourPlot::Q3ContourPlot(Q3Mesh *mesh) :
    mesh_(mesh),
    values_(mesh->nodes().count())
{
    qFill(values_.begin(), values_.end(), 0);
}

void Q3ContourPlot::createContour(int levels)
{
    contours_.clear();
    QVector<qreal> normalizedValues = normalize();
    Q3ContourGenerator contourGenerator(mesh_, normalizedValues);

    levels = levels < 3 ? 3 : levels;
    qreal stepSize = 1. / (levels - 1);

    for (int i = 0; i < levels; ++i)
    {
        qreal level = stepSize * i;
        Q3Contour contour = contourGenerator.createContour(level);
        contours_.append(contour);
    }
}

void Q3ContourPlot::draw(Q3Painter &painter) const
{
    foreach (const Q3Contour& contour, contours_)
        contour.draw(painter);
}

QVector<qreal>& Q3ContourPlot::values()
{
    return values_;
}

qreal Q3ContourPlot::minValue()
{
    qreal minValue = 0;
    for (QVector<qreal>::const_iterator it = values_.begin();
         it != values_.end(); ++it)
    {
        if (it == values_.begin())
        {
            minValue = *it;
            continue;
        }

        if (minValue > *it)
            minValue = *it;
    }
    return minValue;
}

qreal Q3ContourPlot::maxValue()
{
    qreal maxValue = 0;
    for (QVector<qreal>::const_iterator it = values_.begin();
         it != values_.end(); ++it)
    {
        if (it == values_.begin())
        {
            maxValue = *it;
            continue;
        }

        if (maxValue < *it)
            maxValue = *it;
    }
    return maxValue;
}

QVector<qreal> Q3ContourPlot::normalize()
{
    QVector<qreal> normalizedValues = values_;

    qreal minVal = minValue();
    qreal maxVal = maxValue();

    qreal diff = maxVal - minVal;

    for (int i = 0; i < values_.size(); ++i)
    {
        normalizedValues[i] -= minVal;
        if (diff > 0)
            normalizedValues[i] /= diff;
    }

    return normalizedValues;
}
