#include <QDebug>
#include <QTextStream>
#include <QToolTip>

#include "q3plot.h"
#include "q3mesh.h"
#include "q3pointinfodirector.h"

Q3PointInfoDirector::Q3PointInfoDirector(Q3Mesh &mesh, QWidget *parent) :
    Q3Director(Q3Director::PointInfo, parent),
    mesh_(mesh)
{

}

Q3PointInfoDirector::~Q3PointInfoDirector()
{

}

bool Q3PointInfoDirector::processClick(QMouseEvent *event,
                                       const QPointF &scenePos)
{
    if (event->button() != Qt::RightButton)
        return false;

    QToolTip::showText(event->globalPos(), pointInfo(scenePos));
    return true;
}

void Q3PointInfoDirector::stop()
{
    setActive(false);
}

void Q3PointInfoDirector::draw(Q3Painter &painter) const
{
    Q_UNUSED(painter);
}

QString Q3PointInfoDirector::pointInfo(const QPointF &pos)
{
    for (int i = 0; i < mesh_.triangles().count(); ++i)
    {
        Q3MeshTriangle *triangle = mesh_.triangles().at(i);
        QPolygonF polygon = triangle->toPolygonF(1, 1);
        if (polygon.containsPoint(pos, Qt::OddEvenFill))
        {
//            qreal pressure = 0;
//            foreach (Q3MeshEdge *edge, triangle->edges())
//                pressure += edge->pressure();
//            pressure /= triangle->edges().count();

            QString info;
            QTextStream stream(&info);

            stream << trUtf8("Vx: ")
                   << QString::number(triangle->correctorVelocity().x()) << "\n"
                   << trUtf8("Vy: ")
                   << QString::number(triangle->correctorVelocity().y()) << "\n"
                   << trUtf8("Завихренность: ")
                   << QString::number(triangle->vorticity()) << "\n"
                   << trUtf8("Функция тока: ")
                   << QString::number(triangle->stream()) << "\n"
                   << trUtf8("Давление: ")
                   << QString::number(triangle->pressure()) << "\n"
                   << trUtf8("Дивергенция: ")
                   << QString::number(triangle->divergence(true));
            return info;
        }
    }
    return "";
}
