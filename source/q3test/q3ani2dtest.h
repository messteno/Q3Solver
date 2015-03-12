#ifndef Q3ANI2DTEST_H
#define Q3ANI2DTEST_H

#include <QtTest/QtTest>

class Q3Ani2DTest : public QObject
{
    Q_OBJECT
    static void boundary(int *param, double *t, double *x, double *y);
public:
    Q3Ani2DTest();
    ~Q3Ani2DTest();
private slots:
    void ani2DCreateMesh();
};

#endif // Q3ANI2DTEST_H
