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

class Q3PreassurePlot : public QObject, public Q3ContourPlot
{
    Q_OBJECT
public:
    Q3PreassurePlot(Q3Mesh &mesh);

public slots:
    void update(bool init = false);
};

class Q3XYPlot : public Q3PlotDrawable
{
public:
    void draw(Q3Painter &painter) const;
    void setPoints(const QList<QPointF> &points);

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


#endif // Q3GRAPHS_H
