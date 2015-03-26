#include "q3meshadapter.h"

Q3MeshAdapter::Q3MeshAdapter() :
    created_(false),
    sizePolicy_(ElementSizeAuto),
    elementsCount_(20000),
    elementSize_(0.01)
{

}

void Q3MeshAdapter::setSizePolicy(const SizePolicy &sizePolicy)
{
    sizePolicy_ = sizePolicy;
}

void Q3MeshAdapter::setElementsCount(int elementsCount)
{
    elementsCount_ = elementsCount;
}

void Q3MeshAdapter::setElementSize(qreal elementSize)
{
    elementSize_ = elementSize;
}

