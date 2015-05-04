#include "q3contour.h"

Q3Contour::Q3Contour(Q3Mesh *mesh, const QVector<qreal> &values) :
    mesh_(mesh),
    values_(values)
{
    // TODO: исправить на какую-то проверку
    Q_ASSERT(mesh_->triangles().size() == values_.size());
}

Q3Contour::~Q3Contour()
{

}

