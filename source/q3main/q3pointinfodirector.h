#ifndef Q3POINTINFODIRECTOR_H
#define Q3POINTINFODIRECTOR_H

#include <QString>

#include "q3mesh.h"
#include "q3director.h"

class Q3PointInfoDirector : public Q3Director
{
public:
    Q3PointInfoDirector(Q3Mesh &mesh, QWidget *parent = NULL);
    virtual ~Q3PointInfoDirector();
    virtual bool processClick(QMouseEvent *event, const QPointF &scenePos);
    virtual void stop();
    virtual void draw(Q3Painter &painter) const;

private:
    Q3Mesh &mesh_;
    QString pointInfo(const QPointF &pos);
};

#endif // Q3POINTINFODIRECTOR_H
