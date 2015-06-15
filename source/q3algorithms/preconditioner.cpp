#include "preconditioner.h"

Preconditioner::Preconditioner(int size) :
    LinearOperator(size)
{

}

IdentityPreconditioner::IdentityPreconditioner(int size) :
    Preconditioner(size)
{

}

Q3Vector IdentityPreconditioner::operator*(const Q3Vector &vec) const
{
    Q_ASSERT(size_ == vec.size());
    return vec;
}

Q3Vector IdentityPreconditioner::solve(const Q3Vector &b) const
{
    Q_ASSERT(size_ == b.size());
    return b;
}
