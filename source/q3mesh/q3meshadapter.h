#ifndef Q3MESHADAPTER_H
#define Q3MESHADAPTER_H

#include <QList>
#include <QMap>
#include <QObject>

#include "q3mesh.h"
#include "q3sceleton.h"
#include "q3boundary.h"

class Q3MeshAdapter : public QObject
{
    Q_OBJECT

public:
    enum SizePolicy
    {
        ElementSizeAuto,
        ElementSizeByCount,
        ElementSizeBySize,
    };

    Q3MeshAdapter();

    virtual bool generateMesh(Q3Sceleton *sceleton,
                              QList<Q3Boundary *> *boundaries) = 0;
    virtual bool meshToQ3Mesh(Q3Mesh *mesh,
                              QList<Q3Boundary *> *boundaries) = 0;
    virtual bool saveMesh() = 0;

    virtual void setSizePolicy(const SizePolicy &sizePolicy);
    virtual void setElementsCount(int elementsCount);
    virtual void setElementSize(qreal elementSize);

signals:
    void meshCreated();

protected:
    qreal elementSize_;
    int elementsCount_;
    SizePolicy sizePolicy_;
    bool created_;
};

#endif // Q3MESHADAPTER_H

