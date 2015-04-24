#ifndef Q3BOUNDARY_H
#define Q3BOUNDARY_H

#include "q3sceletonitem.h"
#include "q3boundarytype.h"

class Q3Boundary
{
public:
    Q3Boundary();
    ~Q3Boundary();

    void setTypeByEnum(Q3BoundaryType::Type type);
    void setType(Q3BoundaryType *type);
    bool contains(Q3SceletonItem *item);

    void addItem(Q3SceletonItem *item);

    Q3BoundaryType *type() const;
    
    static Q3Boundary* findByElement(QList<Q3Boundary *> *boundaries,
                                     Q3SceletonItem *item);
    static Q3Boundary* findByLabel(QList<Q3Boundary *> *boundaries,
                                   int label);
    static void setUniqLabels(QList<Q3Boundary *> *boundaries);

    int label() const;
    void setLabel(int label);

    QVector2D velocity(QPointF point);

private:
    Q3BoundaryType *type_;
    Q3SceletonItem *item_;
    int label_;
};

#endif // Q3BOUNDARY_H
