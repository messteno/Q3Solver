#include <QtAlgorithms>

#include "q3contourdirector.h"

Q3ContourDirector::Q3ContourDirector(Q3Mesh &mesh, Q3ContourPlot &contourPlot,
                                     QWidget *parent) :
    Q3Director(Q3Director::Contour, parent),
    contourPlot_(contourPlot),
    mesh_(mesh)
{

}

Q3ContourDirector::~Q3ContourDirector()
{

}

bool Q3ContourDirector::processClick(QMouseEvent *event, const QPointF &scenePos)
{
    Q_UNUSED(event);

    for (int i = 0; i < mesh_.triangles().count(); ++i)
    {
        Q3MeshTriangle *triangle = mesh_.triangles().at(i);
        QPolygonF polygon = triangle->toPolygonF(1, 1);
        if (polygon.containsPoint(scenePos, Qt::OddEvenFill))
        {
            QList<qreal> levelsList = contourPlot_.contourLevelsList();
            levelsList.append(triangle->stream());
            qSort(levelsList.begin(), levelsList.end());
            contourPlot_.setContourLevelsList(levelsList);
            contourPlot_.createContour();
            return true;
        }
    }
    return false;
}

void Q3ContourDirector::stop()
{

}

void Q3ContourDirector::draw(Q3Painter &painter) const
{
    Q_UNUSED(painter);
}
