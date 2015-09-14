#ifndef CONJUGATEGRADIENT_H
#define CONJUGATEGRADIENT_H

#include <QVector>

namespace ConjugateGradient
{
const int maxIterations = 2000;
const double eps = 1e-10;

int calculate(double *AN, int *JA, int *IA,
              double *XN, double *BN, double *MN,
              double *TN, int n);
void multiply1(double *AN, int *JA, int *IA,
               double *x, double *b, double *r, int n);
void multiply2(double *AN, int *JA, int *IA,
               double *p, double *q, int n);
double scalar(double *a, double *b, int n);
void add(double *a, double coef, double *b, double *c, int n);
double norm(double *a, int n);
void solve(double *MN, int *JA, int *IA, double *r, double *x, int n);
}

#endif // CONJUGATEGRADIENT_H
