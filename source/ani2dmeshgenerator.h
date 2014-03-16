#ifndef ANI2DMESHGENERATOR_H
#define ANI2DMESHGENERATOR_H

#include "meshgenerator.h"
#include "qani2d.h"

class Ani2DMeshGenerator : public MeshGenerator
{
public:
    Ani2DMeshGenerator();
    virtual ~Ani2DMeshGenerator();
    virtual Mesh *generateMesh(const QList<QMeshItem *>& itemList);
private:
    QAni2D qAni_;
};

#endif // ANI2DMESHGENERATOR_H
