#include "linearoperator.h"

LinearOperator::LinearOperator(int size) :
    size_(size)
{

}

int LinearOperator::size() const
{
    return size_;
}

Q3Vector IdentityOperator::operator *(const Q3Vector &vec) const
{
    return vec;
}
