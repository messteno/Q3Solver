#ifndef BICGSTABLINEARSOLVER_H
#define BICGSTABLINEARSOLVER_H

#include <QVector>

#include "linearoperator.h"
#include "preconditioner.h"
#include "q3vector.h"

class BiCGStabLinearSolver
{
public:
    BiCGStabLinearSolver();
    BiCGStabLinearSolver(const qreal eps, const int maxIterationsCount);

    int solve(const LinearOperator &A,
              Q3Vector &x,
              const Q3Vector &b,
              const Preconditioner &M,
              qreal &residual);

    static const qreal DefaultEps;
    static const int DefaultMaxIteratinsCount;

private:
    qreal eps_;
    qreal maxIterationsCount_;
};

#endif // BICGSTABLINEARSOLVER_H
