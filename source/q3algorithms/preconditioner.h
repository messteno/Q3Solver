#ifndef PRECONDITIONER_H
#define PRECONDITIONER_H

#include "linearoperator.h"
#include "q3vector.h"

class Preconditioner : public LinearOperator
{
public:
    Preconditioner(int size);
    virtual Q3Vector solve(const Q3Vector &b) const = 0;
};

class IdentityPreconditioner : public Preconditioner
{
public:
    IdentityPreconditioner(int size);
    Q3Vector operator*(const Q3Vector &vec) const;
    Q3Vector solve(const Q3Vector &b) const;
};

#endif // PRECONDITIONER_H
