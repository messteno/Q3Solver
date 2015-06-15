#ifndef Q3BICGTEST_H
#define Q3BICGTEST_H

#include <QtTest/QtTest>

#include "q3vector.h"
#include "linearoperator.h"
#include "preconditioner.h"
#include "bicgstablinearsolver.h"

class TestLO : public LinearOperator
{
public:
    TestLO(int size) : LinearOperator(size) {}
    Q3Vector operator *(const Q3Vector &vec) const
    {
        QVector<qreal> res(size_);
        QVector<qreal> A;
        A << 1 << 2;
        A << 3 << 4;

        for (int row = 0; row < size_; ++row)
        {
            qreal sum = 0;
            for (int col = 0; col < size_; ++col)
                sum += A[row * size_ + col] * vec[col];
            res[row] = sum;
        }
        return res;
    }
};

class Q3BiCGTest : public QObject
{
    Q_OBJECT
public:
    Q3BiCGTest();
    ~Q3BiCGTest();

private slots:
    void solveTest();
};

#endif // Q3BICGTEST_H
