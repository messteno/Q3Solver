#ifndef Q3GRAPHS_H
#define Q3GRAPHS_H

#include "q3plot.h"
#include "q3contour.h"

class Q3StreamPlot : public QObject, public Q3ContourPlot
{
    Q_OBJECT
public:
    Q3StreamPlot(Q3Mesh &mesh);

public slots:
    void update(bool init = false);
};

class Q3VorticityPlot: public QObject, public Q3ContourPlot
{
    Q_OBJECT
public:
    Q3VorticityPlot(Q3Mesh &mesh);

public slots:
    void update(bool init = false);
};

class Q3MagnitudePlot: public QObject, public Q3ContourPlot
{
    Q_OBJECT
public:
    Q3MagnitudePlot(Q3Mesh &mesh);

public slots:
    void update(bool init = false);
};

class Q3PressurePlot : public QObject, public Q3ContourPlot
{
    Q_OBJECT
public:
    Q3PressurePlot(Q3Mesh &mesh);

public slots:
    void update(bool init = false);
};

class Q3XYPlot : public Q3PlotDrawable
{
public:
    void draw(Q3Painter &painter) const;
    void setPoints(const QList<QPointF> &points);
    QList<QPointF> points() const;

protected:
    QList<QPointF> points_;
};

class Q3VxByYPlot : public QObject, public Q3XYPlot
{
    Q_OBJECT
public:
    Q3VxByYPlot(Q3Mesh &mesh);
    Q3VxByYPlot(Q3Mesh &mesh, qreal x0);
    QRectF boundingRect() const;
    void setXValue(const qreal &xValue);

public slots:
    void update();

private:
    Q3Mesh &mesh_;
    QRectF boundingRect_;
    qreal xValue_;
};

class Q3VyByXPlot : public QObject, public Q3XYPlot
{
    Q_OBJECT
public:
    Q3VyByXPlot(Q3Mesh &mesh);
    QRectF boundingRect() const;
    void setYValue(const qreal &yValue);

public slots:
    void update();

private:
    Q3Mesh &mesh_;
    QRectF boundingRect_;
    qreal yValue_;
};

class Q3RealTimePlot : public Q3PlotDrawable
{
public:
    Q3RealTimePlot();
    void addValue(qreal time, qreal value);
    void draw(Q3Painter &painter) const;
    void setTimeDelta(const qreal &timeDelta);
    QRectF boundingRect() const;

protected:
    qreal timeDelta_;
    QVector<QPair<qreal, qreal> > timeValues_;
    QRectF boundingRect_;
};

class Q3CdRealTimePlot : public QObject, public Q3RealTimePlot
{
    Q_OBJECT
public:
    Q3CdRealTimePlot(Q3Mesh &mesh, Q3Mesh::EdgeBoundary &boundary, Q3Plot &plot, qreal Re);

public slots:
    void update(qreal time);

private:
    Q3Mesh &mesh_;
    Q3Mesh::EdgeBoundary &boundary_;
    Q3Plot &plot_;
    qreal Re_;
};

class Q3ClRealTimePlot : public QObject, public Q3RealTimePlot
{
    Q_OBJECT
public:
    Q3ClRealTimePlot(Q3Mesh &mesh, Q3Mesh::EdgeBoundary &boundary, Q3Plot &plot, qreal Re);

public slots:
    void update(qreal time);

private:
    Q3Mesh &mesh_;
    Q3Mesh::EdgeBoundary &boundary_;
    Q3Plot &plot_;
    qreal Re_;
};

#endif // Q3GRAPHS_H
