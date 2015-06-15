#include <QVector3D>
#include <QVector>
#include <QDebug>
#include <QFile>

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

Q3PreassurePlot::Q3PreassurePlot(Q3Mesh &mesh) :
    Q3ContourPlot(mesh)
{
    update(true);
}

void Q3PreassurePlot::update(bool init)
{
    QVector<QVector3D> edgeValues;
    for (int i = 0; i < mesh_.edges().count(); ++i)
    {
        Q3MeshEdge *edge = mesh_.edges().at(i);
        edgeValues.append(QVector3D(edge->center().x(),
                                    edge->center().y(),
                                    edge->preassure()));
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


Q3VxByYPlot::Q3VxByYPlot(Q3Mesh &mesh) :
    mesh_(mesh),
    xValue_(mesh.boundingRect().center().x())
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
        qreal y = interpolation.interpolateToPoint(
                      QPointF(xValue_, x));
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
    qDebug() << boundingRect_;
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
