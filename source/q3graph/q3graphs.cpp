#include <QVector3D>
#include <QVector>
#include <QDebug>
#include <QFile>
#include <qmath.h>

#include "q3graphs.h"
#include "q3meshinterpolation.h"

Q3StreamPlot::Q3StreamPlot(Q3Mesh &mesh) :
    Q3ContourPlot(mesh)
{
    update(true);
}

void Q3StreamPlot::update(bool init)
{
    mesh_.calcStream();

    QVector<QVector3D> triValues;
    for (int i = 0; i < mesh_.triangles().count(); ++i)
    {
        Q3MeshTriangle *triangle = mesh_.triangles().at(i);
        triValues.append(QVector3D(triangle->center().x(),
                                   triangle->center().y(),
                                   triangle->stream()));
    }
    Q3MeshTriNodeInterpolation interpolation(mesh_, triValues);
    QVector<qreal> nodeValues = interpolation.interpolateToNodes();

    clear();
    setValues(nodeValues, init);
    createFilledContour();
    createContour();
}

Q3VorticityPlot::Q3VorticityPlot(Q3Mesh &mesh) :
    Q3ContourPlot(mesh)
{
    update(true);
}

void Q3VorticityPlot::update(bool init)
{
    mesh_.calcVorticity();

    QVector<QVector3D> triValues;
    for (int i = 0; i < mesh_.triangles().count(); ++i)
    {
        Q3MeshTriangle *triangle = mesh_.triangles().at(i);
        triValues.append(QVector3D(triangle->center().x(),
                                   triangle->center().y(),
                                   triangle->vorticity()));
    }
    Q3MeshTriNodeInterpolation interpolation(mesh_, triValues);
    QVector<qreal> nodeValues = interpolation.interpolateToNodes();

    for (int i = 0; i < nodeValues.count(); ++i)
    {
        Q3MeshNode *node = mesh_.nodes().at(i);
        if (node->boundary() && qAbs(node->y() + 0.5) < 1e-10)
            qDebug() << node->x() << nodeValues.at(i);
    }

    clear();
    setValues(nodeValues, init);
    createFilledContour();
    createContour();
}

Q3MagnitudePlot::Q3MagnitudePlot(Q3Mesh &mesh) :
    Q3ContourPlot(mesh)
{
    update(true);
}

void Q3MagnitudePlot::update(bool init)
{
    QVector<QVector3D> triValues;
    for (int i = 0; i < mesh_.triangles().count(); ++i)
    {
        Q3MeshTriangle *triangle = mesh_.triangles().at(i);
        triValues.append(QVector3D(triangle->center().x(),
                                   triangle->center().y(),
                                   triangle->correctorVelocity().length()));
    }
    Q3MeshTriNodeInterpolation interpolation(mesh_, triValues);
    QVector<qreal> nodeValues = interpolation.interpolateToNodes();

    clear();
    setValues(nodeValues, init);
    createFilledContour();
    createContour();
}

Q3PressurePlot::Q3PressurePlot(Q3Mesh &mesh) :
    Q3ContourPlot(mesh)
{
    update(true);
}

void Q3PressurePlot::update(bool init)
{
    QVector<QVector3D> edgeValues;
    for (int i = 0; i < mesh_.edges().count(); ++i)
    {
        Q3MeshEdge *edge = mesh_.edges().at(i);
        edgeValues.append(QVector3D(edge->center().x(),
                                    edge->center().y(),
                                    edge->pressure()));
    }
    Q3MeshEdgeNodeInterpolation interpolation(mesh_, edgeValues);
    QVector<qreal> nodeValues = interpolation.interpolateToNodes();

    clear();
    setValues(nodeValues, init);
    createFilledContour();
    createContour();
}

void Q3XYPlot::draw(Q3Painter &painter) const
{
    if (points_.empty())
        return;

    painter.save();
    // TODO: посмотреть, кто меняет цвет
    painter.setPen(Qt::black);
    painter.setBrush(Qt::NoBrush);
    qreal sx = painter.sx();
    qreal sy = painter.sy();

    QPainterPath path;

    path.moveTo(points_.at(0).x() * sx, points_.at(0).y() * sy);
    for (int i = 1; i < points_.size(); ++i)
        path.lineTo(points_.at(i).x() * sx, points_.at(i).y() * sy);
    painter.drawPath(path);
    painter.restore();
}

void Q3XYPlot::setPoints(const QList<QPointF> &points)
{
    points_ = points;
}

QList<QPointF> Q3XYPlot::points() const
{
    return points_;
}

Q3VxByYPlot::Q3VxByYPlot(Q3Mesh &mesh) :
    mesh_(mesh),
    xValue_(mesh.boundingRect().center().x())
{
    update();
}

Q3VxByYPlot::Q3VxByYPlot(Q3Mesh &mesh, qreal x0) :
    mesh_(mesh),
    xValue_(x0)
{
    update();
}

void Q3VxByYPlot::update()
{
    QVector<QVector3D> values;
    for (int i = 0; i < mesh_.triangles().count(); ++i)
    {
        Q3MeshTriangle *triangle = mesh_.triangles().at(i);
        values.append(QVector3D(triangle->center().x(),
                                triangle->center().y(),
                                triangle->correctorVelocity().x()));
    }

    for (int i = 0; i < mesh_.edges().count(); ++i)
    {
        Q3MeshEdge *edge = mesh_.edges().at(i);
        if (!edge->boundary())
            continue;
        values.append(QVector3D(edge->center().x(),
                                edge->center().y(),
                                edge->velocity().x()));
    }

    NaturalNeigbourInterpolation interpolation(values);

    QRectF meshRect = mesh_.boundingRect();

    points_.clear();
    int steps = 100;
    qreal step = meshRect.height() / (steps - 1.);

    qreal minX, minY, maxX, maxY;

    for (int i = 0; i < steps; ++i)
    {
        qreal x = meshRect.top() + i * step;
        qreal y = interpolation.interpolateToPoint(QPointF(xValue_, x));
        points_.append(QPointF(x, y));

        if (i == 0)
        {
            minX = maxX = x;
            minY = maxY = y;
        }
        else
        {
            if (x < minX)
                minX = x;
            if (x > maxX)
                maxX = x;
            if (y < minY)
                minY = y;
            if (y > maxY)
                maxY = y;
        }
    }
    boundingRect_ = QRectF(minX, minY, maxX - minX, maxY - minY);
}

void Q3VxByYPlot::setXValue(const qreal &xValue)
{
    xValue_ = xValue;
}

QRectF Q3VxByYPlot::boundingRect() const
{
    return boundingRect_;
}

Q3VyByXPlot::Q3VyByXPlot(Q3Mesh &mesh) :
    mesh_(mesh),
    yValue_(mesh.boundingRect().center().y())
{
    update();
}

void Q3VyByXPlot::update()
{
    QVector<QVector3D> values;
    for (int i = 0; i < mesh_.triangles().count(); ++i)
    {
        Q3MeshTriangle *triangle = mesh_.triangles().at(i);
        values.append(QVector3D(triangle->center().x(),
                                triangle->center().y(),
                                triangle->correctorVelocity().y()));
    }

    for (int i = 0; i < mesh_.edges().count(); ++i)
    {
        Q3MeshEdge *edge = mesh_.edges().at(i);
        if (!edge->boundary())
            continue;
        values.append(QVector3D(edge->center().x(),
                                edge->center().y(),
                                edge->velocity().y()));
    }

    NaturalNeigbourInterpolation interpolation(values);

    QRectF meshRect = mesh_.boundingRect();

    points_.clear();
    int steps = 100;
    qreal step = meshRect.width() / (steps - 1.);

    qreal minX, minY, maxX, maxY;

    for (int i = 0; i < steps; ++i)
    {
        qreal x = meshRect.left() + i * step;
        qreal y = interpolation.interpolateToPoint(
                      QPointF(x, yValue_));
        points_.append(QPointF(x, y));

        if (i == 0)
        {
            minX = maxX = x;
            minY = maxY = y;
        }
        else
        {
            if (x < minX)
                minX = x;
            if (x > maxX)
                maxX = x;
            if (y < minY)
                minY = y;
            if (y > maxY)
                maxY = y;
        }
    }
    boundingRect_ = QRectF(minX, minY, maxX - minX, maxY - minY);
}

void Q3VyByXPlot::setYValue(const qreal &yValue)
{
    yValue_ = yValue;
}

QRectF Q3VyByXPlot::boundingRect() const
{
    return boundingRect_;
}

Q3RealTimePlot::Q3RealTimePlot() :
    timeDelta_(1),
    boundingRect_(0, 0, 1, 1)
{

}

void Q3RealTimePlot::addValue(qreal time, qreal value)
{
    qreal currentTime = time;
    if (!timeValues_.empty())
        currentTime = timeValues_.last().first;

    if (currentTime > time)
        return;

    timeValues_.append(QPair<qreal, qreal>(time, value));

    qreal firstTime = timeValues_.first().first;
    if (firstTime < currentTime - timeDelta_)
        firstTime = currentTime - timeDelta_;
    QMutableVectorIterator<QPair<qreal, qreal> > it(timeValues_);
    while (it.hasNext())
    {
        QPair<qreal, qreal> tv = it.next();
        if (tv.first < firstTime - 0.1 * timeDelta_)
            it.remove();
        else
            break;
    }

    qreal minValue = timeValues_.first().second;
    qreal maxValue = minValue;

    for (int i = 0; i < timeValues_.count(); ++i)
    {
        qreal value = timeValues_.at(i).second;
        if (value < minValue)
            minValue = value;
        if (value > maxValue)
            maxValue = value;
    }

    boundingRect_ = QRectF(firstTime, minValue, timeDelta_, maxValue - minValue);
}

void Q3RealTimePlot::draw(Q3Painter &painter) const
{
    if (timeValues_.empty())
        return;

    painter.save();
    // TODO: посмотреть, кто меняет цвет
    painter.setPen(Qt::black);
    painter.setBrush(Qt::NoBrush);
    qreal sx = painter.sx();
    qreal sy = painter.sy();

    QPainterPath path;

    for (int i = 0; i < timeValues_.size(); ++i)
    {
        qreal time = timeValues_.at(i).first;
        qreal value = timeValues_.at(i).second;
        if (i == 0)
        {
            path.moveTo(time * sx, value * sy);
            continue;
        }
        path.lineTo(time * sx, value * sy);
    }
    painter.drawPath(path);
    painter.restore();
}

void Q3RealTimePlot::setTimeDelta(const qreal &timeDelta)
{
    timeDelta_ = timeDelta;
}

QRectF Q3RealTimePlot::boundingRect() const
{
    return boundingRect_;
}

Q3CdRealTimePlot::Q3CdRealTimePlot(Q3Mesh &mesh, Q3Mesh::EdgeBoundary &boundary,
                                   Q3Plot &plot, qreal Re) :
    mesh_(mesh),
    boundary_(boundary),
    plot_(plot),
    Re_(Re)
{
}

void Q3CdRealTimePlot::update(qreal time)
{
    qreal Fd = 0;
    foreach (Q3MeshEdge *edge, boundary_)
    {
        Q3MeshTriangle *triangle = edge->adjacentTriangles().at(0);
        QVector2D trV = triangle->correctorVelocity();
        QVector2D normal = edge->normalVector();
        QVector2D tangentVector = QVector2D(normal.y(), -normal.x());
        Fd += edge->length()
              * (QVector2D::dotProduct(trV, tangentVector)
                 / QVector2D(triangle->center() - edge->center()).length() * normal.y() / Re_
                 - edge->pressure() * normal.x());
    }
    qreal Cd = 2 * Fd * 10.;

    addValue(time, Cd);
    if (timeValues_.size() <= 2)
        plot_.setSceneRect(boundingRect_);
    plot_.update();
}

Q3ClRealTimePlot::Q3ClRealTimePlot(Q3Mesh &mesh, Q3Mesh::EdgeBoundary &boundary,
                                   Q3Plot &plot, qreal Re) :
    mesh_(mesh),
    boundary_(boundary),
    plot_(plot),
    Re_(Re)
{
}

void Q3ClRealTimePlot::update(qreal time)
{
    qreal Fl = 0;
    foreach (Q3MeshEdge *edge, boundary_)
    {
        Q3MeshTriangle *triangle = edge->adjacentTriangles().at(0);
        QVector2D trV = triangle->correctorVelocity();
        QVector2D normal = edge->normalVector();
        QVector2D tangentVector = QVector2D(normal.y(), -normal.x());
        Fl += -edge->length()
              * (QVector2D::dotProduct(trV, tangentVector)
                 / QVector2D(triangle->center() - edge->center()).length() * normal.x() / Re_
                 - edge->pressure() * normal.y());
    }
    qreal Cl = 2 * Fl * 10.;

    addValue(time, Cl);
    if (timeValues_.size() <= 2)
        plot_.setSceneRect(boundingRect_);
    plot_.update();
}
