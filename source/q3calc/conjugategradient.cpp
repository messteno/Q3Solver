#include <math.h>

#include "conjugategradient.h"

int ConjugateGradient::calculate(double *AN, int *JA, int *IA,
                                 double *XN, double *BN, double *MN,
                                 double *TN, int n)
{
    double *p = TN;
    double *q = TN + n;
    double *r = TN + 2 * n;
    double *z = TN + 3 * n;
    double rho, rho_1, alpha, beta;
    int it = 0;
    double ce = 1.;

    ConjugateGradient::multiply1(AN, JA, IA, XN, BN, r, n);

    while (it != ConjugateGradient::maxIterations)
    {
        ConjugateGradient::solve(MN, JA, IA, r, z, n);

        rho = ConjugateGradient::scalar(z, r, n);
        if (it == 0)
            memcpy (p, z, n * sizeof (double));
        else
        {
            beta = rho / rho_1;
            ConjugateGradient::add(z, beta, p, p, n);
        }

        ConjugateGradient::multiply2(AN, JA, IA, p, q, n);

        alpha = rho / ConjugateGradient::scalar(p, q, n);

        ConjugateGradient::add(XN, alpha, p, XN, n);
        ConjugateGradient::add(r, - alpha, q, r, n);

        rho_1 = rho;

        it ++;

        if (ConjugateGradient::norm(r, n) < ConjugateGradient::eps)
            break;
        if (ConjugateGradient::norm(r, n) < ce)
            ce /= 10.;
    }
    return it;
}

void ConjugateGradient::multiply1(double *AN, int *JA, int *IA,
                                  double *x, double *b, double *r, int n)
{
    memset (r, 0, n * sizeof (double));
    for (int i = 0; i < n; ++i)
    {
        r[i] += b[i];
        int j = IA[i];
        r[i] -= AN[j] * x[JA[j]];
        for (j = j + 1; j < IA[i + 1]; ++j)
        {
            r[i] -= AN[j] * x[JA[j]];
            r[JA[j]] -= AN[j] * x[i];
        }
    }
}

void ConjugateGradient::multiply2(double *AN, int *JA, int *IA,
                                  double *p, double *q, int n)
{
    memset(q, 0, n * sizeof (double));
    for (int i = 0; i < n; ++i)
    {
        int j = IA[i];
        q[i] += AN[j] * p[JA[j]];
        for (j = j + 1; j < IA[i + 1]; ++j)
        {
            q[i] += AN[j] * p[JA[j]];
            q[JA[j]] += AN[j] * p[i];
        }
    }
}

double ConjugateGradient::scalar(double *a, double *b, int n)
{
    double sc = 0.;

    for (int i = 0; i < n; ++i)
        sc += a[i] * b[i];
    return sc;
}


void ConjugateGradient::add(double *a, double coef, double *b, double *c, int n)
{
    for (int i = 0; i < n; ++i)
        c[i] = a[i] + coef * b[i];
}

double ConjugateGradient::norm(double *a, int n)
{
    double max = 0.;
    for (int i = 0; i < n; ++i)
    {
        if (fabs(a[i]) > max)
            max = fabs(a[i]);
    }
    return max;
}

void ConjugateGradient::solve(double *MN, int *JA, int *IA, double *r,
                              double *x, int n)
{
    memcpy(x, r, n * sizeof(double));
    for (int i = 0; i < n; ++i)
    {
        int j = IA[i];
        x[i] /= MN[j];
        for (j = j + 1; j < IA[i + 1]; ++j)
            x[JA[j]] -= x[i] * MN[j];
    }

    for (int i = n - 1; i >= 0; --i)
    {
        for (int j = IA[i] + 1; j < IA[i + 1]; ++j)
            x[i] -= MN[j] * x[JA[j]];
        x[i] /= MN[IA[i]];
    }
}
