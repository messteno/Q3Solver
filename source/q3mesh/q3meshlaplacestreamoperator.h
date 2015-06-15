#ifndef Q3MESHLAPLACESTREAMOPERATOR_H
#define Q3MESHLAPLACESTREAMOPERATOR_H

#include "q3mesh.h"
#include "linearoperator.h"

class Q3MeshLaplaceStreamOperator : public LinearOperator
{
public:
    Q3MeshLaplaceStreamOperator(Q3Mesh &mesh, int size);
    Q3Vector operator *(const Q3Vector &vec) const;

private:
    Q3Mesh &mesh_;
};

#endif // Q3MESHLAPLACESTREAMOPERATOR_H
