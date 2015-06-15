#include <QDebug>

#include "bicgstablinearsolver.h"

const qreal BiCGStabLinearSolver::DefaultEps = 1e-8;
const int BiCGStabLinearSolver::DefaultMaxIteratinsCount = 1000;

BiCGStabLinearSolver::BiCGStabLinearSolver() :
    eps_(DefaultEps),
    maxIterationsCount_(DefaultMaxIteratinsCount)
{

}

BiCGStabLinearSolver::BiCGStabLinearSolver(const qreal eps,
                                           const int maxIterationsCount) :
    eps_(eps),
    maxIterationsCount_(maxIterationsCount)
{

}

int BiCGStabLinearSolver::solve(const LinearOperator &A,
                                Q3Vector &x,
                                const Q3Vector &b,
                                const Preconditioner &M,
                                qreal &residual)
{
    Q_ASSERT(A.size() == b.size());
    Q_ASSERT(b.size() == x.size());
    Q_ASSERT(A.size() == M.size());

    Q3Vector r = b - A * x;
    Q3Vector rTilde = r;

    qreal normb = b.norm();
    if (normb <= 0)
        normb = 1;

    residual = r.norm() / normb;

    if (residual <= eps_)
        return 0;

    qreal rho_2 = 1;
    qreal alpha = 1;
    qreal omega = 1;

    Q3Vector p(A.size());
    Q3Vector v(A.size());

    for (int it = 1; it < maxIterationsCount_; ++it)
    {
        qreal rho_1 = Q3Vector::dot(rTilde, r);
        if (qAbs(rho_1) <= 0)
        {
            residual = r.norm() / normb;
            return it;
        }

        if (it == 1)
            p = r;
        else
        {
            qreal beta = (rho_1 / rho_2) * (alpha / omega);
            p = r + beta * (p - omega * v);
        }

        Q3Vector phat = M.solve(p);
        v = A * phat;
        alpha = rho_1 / Q3Vector::dot(rTilde, v);

        Q3Vector s = r - alpha * v;
        residual = s.norm() / normb;
        if (residual < eps_)
        {
            x += alpha *  phat;
            return it;
        }

        Q3Vector shat = M.solve(s);
        Q3Vector t = A * shat;
        omega = Q3Vector::dot(t, s) / Q3Vector::dot(t, t);

        x += alpha * phat + omega * shat;
        r = s - omega * t;

        rho_2 = rho_1;

        residual = r.norm() / normb;
        if (residual < eps_)
            return it;
    }

    return maxIterationsCount_;
}

