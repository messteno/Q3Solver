#ifndef Q3BOUNDARYTYPE_H
#define Q3BOUNDARYTYPE_H

#include <QWidget>
#include <QString>

#include "q3itemvisitor.h"

class Q3BoundaryType : public QWidget
{
public:
    enum Type
    {
        NoSlipBoundary,
        InBoundary,
        OutBoundary,
        FixedVelocity,
    };

    Q3BoundaryType(Type toEnum, QWidget *parent = NULL);
    virtual ~Q3BoundaryType();

    Type toEnum();

    static QList<Type> supportedBoundaryTypes(Q3SceletonItem::Type toEnum);
    static Q3BoundaryType* getBoundaryTypeByEnum(Q3BoundaryType::Type toEnum,
                                                 QWidget *parent = NULL);
    static QString typeToString(Q3BoundaryType::Type toEnum);

private:
    Type type_;
};

#endif
