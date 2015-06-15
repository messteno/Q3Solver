#ifndef Q3CONTOURDIRECTOR_H
#define Q3CONTOURDIRECTOR_H

#include "q3mesh.h"
#include "q3director.h"
#include "q3contour.h"

class Q3ContourDirector : public Q3Director
{
public:
    Q3ContourDirector(Q3Mesh &mesh, Q3ContourPlot &contourPlot, QWidget *parent = NULL);
    virtual ~Q3ContourDirector();
    virtual bool processClick(QMouseEvent *event, const QPointF &scenePos);
    virtual void stop();
    virtual void draw(Q3Painter &painter) const;

private:
    Q3Mesh &mesh_;
    Q3ContourPlot &contourPlot_;
};


#endif // Q3CONTOURDIRECTOR_H
