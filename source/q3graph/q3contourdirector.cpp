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
    if (event->button() != Qt::LeftButton)
        return false;

    return contourPlot_.addContourAtPoint(scenePos);
}

void Q3ContourDirector::stop()
{

}

void Q3ContourDirector::draw(Q3Painter &painter) const
{
    Q_UNUSED(painter);
}
