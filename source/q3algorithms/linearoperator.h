#ifndef LINEAROPERATOR_H
#define LINEAROPERATOR_H

#include "q3vector.h"

class LinearOperator
{
public:
    LinearOperator(int size);
    virtual Q3Vector operator *(const Q3Vector &vec) const = 0;

    int size() const;

protected:
    int size_;
};

class IdentityOperator : public LinearOperator
{
    Q3Vector operator *(const Q3Vector &vec) const;
};

#endif // LINEAROPERATOR_H
