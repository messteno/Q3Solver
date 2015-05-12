#include <QSet>
#include <QDebug>

#include "q3boundarytype.h"
#include "q3boundaryin.h"
#include "q3boundaryout.h"
#include "q3boundarynoslip.h"
#include "q3boundaryfixedvelocity.h"

Q3BoundaryType::Q3BoundaryType(Type type, QWidget *parent) :
    QWidget(parent),
    type_(type)
{

}

Q3BoundaryType::~Q3BoundaryType()
{
}

QVector2D Q3BoundaryType::velocity(Q3SceletonItem *item, QPointF point)
{
   return QVector2D(0, 0);
}

Q3BoundaryType::Type Q3BoundaryType::toEnum()
{
    return type_;
}

QList<Q3BoundaryType::Type> Q3BoundaryType::supportedBoundaryTypes(
        Q3SceletonItem::Type type)
{
    QList<Q3BoundaryType::Type> types;
    switch(type)
    {
        case Q3SceletonItem::PointConnection:
            types << InBoundary << OutBoundary << NoSlipBoundary << FixedVelocity;
            break;
        case Q3SceletonItem::Circle:
            types << NoSlipBoundary;
    }
    return types;
}

Q3BoundaryType *Q3BoundaryType::getBoundaryTypeByEnum(Q3BoundaryType::Type type,
                                                      QWidget *parent)
{
    switch(type)
    {
        case InBoundary:
            return new Q3BoundaryIn(parent);
        case OutBoundary:
            return new Q3BoundaryOut(parent);
        case NoSlipBoundary:
            return new Q3BoundaryNoSlip(parent);
        case FixedVelocity:
            return new Q3BoundaryFixedVelocity(parent);
    }
    return NULL;
}

QString Q3BoundaryType::typeToString(Q3BoundaryType::Type type)
{
    switch(type)
    {
        case InBoundary:
            return "Вток";
        case OutBoundary:
            return "Сток";
        case NoSlipBoundary:
            return "Непротекание и прилипание";
        case FixedVelocity:
            return "Заданная скорость";
    }
    return "";
}

void Q3BoundaryType::save()
{

}
