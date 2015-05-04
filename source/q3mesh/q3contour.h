#ifndef Q3CONTOUR_H
#define Q3CONTOUR_H

#include "q3mesh.h"

class Q3Contour
{
public:
    Q3Contour(Q3Mesh *mesh, const QVector<qreal> &values);
    ~Q3Contour();

private:
    Q3Mesh *mesh_;
    const QVector<qreal> &values_;
//    QVector<bool>
};

#endif // Q3CONTOUR_H
