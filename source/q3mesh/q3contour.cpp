#include <QDebug>

#include "q3contour.h"

Q3ContourGenerator::Q3ContourGenerator(Q3Mesh *mesh,
                                       const QVector<qreal> &values) :
    mesh_(mesh),
    values_(values),
    interiorVisited_(2 * mesh->triangles().count()),
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
    clearVisitedFlags(false);

    Q3Contour contour(false);
    findBoundaryLines(contour, level);
    findInteriorLines(contour, level, false, false);

    return contour;
}

Q3Contour Q3ContourGenerator::createFilledContour(qreal lowerLevel,
                                                  qreal upperLevel)
{
    clearVisitedFlags(true);

    Q3Contour contour(true);
    findBoundaryLinesFilled(contour, lowerLevel, upperLevel);
    findInteriorLines(contour, lowerLevel, false, true);
    findInteriorLines(contour, upperLevel, true, true);

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

void Q3ContourGenerator::findBoundaryLinesFilled(Q3Contour &contour,
                                                 qreal lowerLevel,
                                                 qreal upperLevel)
{
    Q3Mesh::EdgeBoundaries &boundaries = mesh_->boundaries();
    for (int i = 0; i < boundaries.size(); ++i)
    {
        Q3Mesh::EdgeBoundary &boundary = boundaries[i];
        for (int j = 0; j < boundary.size(); ++j)
        {
            if (boundariesVisited_[i][j])
                continue;

            Q3MeshEdge *edge = boundary[j];
            qreal zStart = values_.at(edge->a()->id());
            qreal zEnd = values_.at(edge->b()->id());

            // Does this boundary edge's z increase through upper level
            // and/or decrease through lower level?
            bool incrUpper = (zStart < upperLevel && zEnd >= upperLevel);
            bool decrLower = (zStart >= lowerLevel && zEnd < lowerLevel);

            if (decrLower || incrUpper)
            {
                contour.append(Q3ContourLine());
                Q3ContourLine &contourLine = contour.last();
                Q3MeshEdge *startEdge = boundary[j];
                Q3MeshEdge *edge = startEdge;

                bool onUpper = incrUpper;
                do
                {
                    Q3MeshTriangle* triangle = edge->adjacentTriangles().at(0);
                    followInterior(contourLine, triangle, edge, true,
                                   onUpper ? upperLevel : lowerLevel, onUpper);
                    onUpper = followBoundary(contourLine, edge, lowerLevel,
                                             upperLevel, onUpper);
                } while (edge != startEdge);

                if (contourLine.count() > 1 &&
                    contourLine.first() == contourLine.last())
                {
                    contourLine.removeLast();
                }
            }
        }
    }

    for (int i = 0; i < boundaries.size(); ++i)
    {
        if (boundariesUsed_[i])
            continue;

        Q3Mesh::EdgeBoundary &boundary = boundaries[i];
        Q3MeshEdge *edge = boundary[0];
        qreal z = values_[edge->a()->id()];
        if (z >= lowerLevel && z <= upperLevel)
        {
            contour.append(Q3ContourLine());
            Q3ContourLine &contourLine = contour.last();
            for (int j = 0; j < boundary.size(); ++j)
                contourLine.append(*edge->a());
        }
    }
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

//        if (!filled)
            contourLine.append(contourLine.first());
//        else if (contourLine.count() > 1 &&
//                 contourLine.first() == contourLine.last())
//        {
//            contourLine.removeLast();
//        }
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

bool Q3ContourGenerator::followBoundary(Q3ContourLine &contourLine,
                                        Q3MeshEdge* &edge, qreal lowerLevel,
                                        qreal upperLevel, bool onUpper)
{
    Q3Mesh::EdgeBoundaries &boundaries = mesh_->boundaries();
    int boundaryIndex, edgeIndex = -1;

    // Может быть передавать в параметрах?
    for (int i = 0; i < boundaries.size(); ++i)
    {
        Q3Mesh::EdgeBoundary &boundary = boundaries[i];
        if (boundary.contains(edge))
        {
            boundaryIndex = i;
            edgeIndex = boundary.indexOf(edge);
            break;
        }
    }
    Q_ASSERT(boundaryIndex != -1 && edgeIndex != -1);
//    qDebug() << boundaryIndex << edgeIndex;
    Q3Mesh::EdgeBoundary &boundary = boundaries[boundaryIndex];

    bool stop = false;
    bool firstEdge = true;
    qreal zStart , zEnd = 0;

    while (!stop)
    {
        Q_ASSERT(!boundariesVisited_[boundaryIndex][edgeIndex]);
        boundariesVisited_[boundaryIndex][edgeIndex] = true;

//        qDebug() << boundaryIndex << edgeIndex;

        if (firstEdge)
            zStart = values_[edge->a()->id()];
        else
            zStart = zEnd;

        zEnd = values_[edge->b()->id()];
        if (zEnd > zStart)
        {
            if (!(!onUpper && firstEdge)
                && zEnd >= lowerLevel && zStart < lowerLevel)
            {
                stop = true;
                onUpper = false;
            }
            else if (zEnd >= upperLevel && zStart < upperLevel)
            {
                stop = true;
                onUpper = true;
            }
        }
        else
        {
            if (!(onUpper && firstEdge)
                && zStart >= upperLevel && zEnd < upperLevel)
            {
                stop = true;
                onUpper = true;
            }
            else if (zStart >= lowerLevel && zEnd < lowerLevel)
            {
                stop = true;
                onUpper = false;
            }
        }

        firstEdge = false;

        if (!stop)
        {
            edgeIndex = (edgeIndex + 1) % (int)boundary.size();
            edge = boundary[edgeIndex];
            contourLine.append(*edge->a());
        }
    }

    return onUpper;
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
    QList<Q3ContourLine>(),
    filled_(false)
{

}

Q3Contour::Q3Contour(bool filled) :
    QList<Q3ContourLine>(),
    filled_(filled)
{

}

void Q3Contour::draw(Q3Painter &painter) const
{
    qreal scaleX = painter.sx();
    qreal scaleY = painter.sy();

    if (filled_)
    {
        painter.setPen(Qt::NoPen);
//        painter.setPen(Qt::gray);
        painter.setBrush(color_);
    }
    else
    {
        painter.setPen(Qt::black);
        painter.setBrush(Qt::NoBrush);
    }

    QPainterPath path;
    for (int i = 0; i < this->size(); ++i)
    {
        const Q3ContourLine &line = this->at(i);
        if (line.empty())
            continue;

        QPointF p = line.at(0);
        path.moveTo(QPointF(p.x() * scaleX, p.y() * scaleY));
        for (int j = 1; j < line.size(); ++j)
        {
            p = line.at(j);
            path.lineTo(p.x() * scaleX, p.y() * scaleY);
        }
    }
    painter.drawPath(path);
}

void Q3Contour::setColor(const QColor &color)
{
    color_ = color;
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

void Q3ContourPlot::createFilledContour(int levels)
{
    filledContours_.clear();
    QVector<qreal> normalizedValues = normalize();
    Q3ContourGenerator contourGenerator(mesh_, normalizedValues);

    levels = levels < 3 ? 3 : levels;
    qreal stepSize = 1. / (levels - 1);

    for (int i = 0; i < levels - 1; ++i)
    {
        // TODO: Подумать над корректным удалением линий
        qreal fixDelta = 5e-3;
        qreal lowerLevel = stepSize * i - fixDelta;
        qreal upperLevel = lowerLevel + stepSize + fixDelta;
        Q3Contour contour = contourGenerator.createFilledContour(lowerLevel,
                                                                 upperLevel);
        qreal level = (lowerLevel + upperLevel) * 0.5;
        contour.setColor(getColour(level));
        filledContours_.append(contour);
    }
}

void Q3ContourPlot::draw(Q3Painter &painter) const
{
    foreach (const Q3Contour& contour, filledContours_)
        contour.draw(painter);

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
void Q3ContourPlot::setValues(const QVector<qreal> &values)
{
    values_ = values;
}

bool Q3ContourPlot::clear()
{
    filledContours_.clear();
    contours_.clear();
}

QColor getColour(qreal level)
{
    QColor color(255, 255, 255);
    qreal dr, dg, db;

    if (level < 0.1242)
    {
        db = 0.504 + ((1. - 0.504) / 0.1242)*level;
        dg = dr = 0.;
    }
    else if (level < 0.3747)
    {
        db = 1.;
        dr = 0.;
        dg = (level - 0.1242) * (1. / (0.3747 - 0.1242));
    }
    else if (level < 0.6253)
    {
        db = (0.6253 - level) * (1. / (0.6253 - 0.3747));
        dg = 1.;
        dr = (level - 0.3747) * (1. / (0.6253 - 0.3747));
    }
    else if (level < 0.8758)
    {
        db = 0.;
        dr = 1.;
        dg = (0.8758 - level) * (1. / (0.8758 - 0.6253));
    }
    else
    {
        db = 0.;
        dg = 0.;
        dr = 1. - (level - 0.8758) * ((1. - 0.504) / (1. - 0.8758));
    }

    color.setRed(dr * 255);
    color.setGreen(dg * 255);
    color.setBlue(db * 255);

    return color;
}
