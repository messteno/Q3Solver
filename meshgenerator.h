#ifndef MESHGENERATOR_H
#define MESHGENERATOR_H

#include "mesh.h"
#include "qmeshitem.h"

class MeshGenerator
{
public:
    MeshGenerator();
    virtual ~MeshGenerator();
    virtual Mesh* generateMesh(const QList<QMeshItem *>& itemList) = 0;
};

#endif // MESHGENERATOR_H
