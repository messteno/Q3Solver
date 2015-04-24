#ifndef Q3CALCTEST_H
#define Q3CALCTEST_H

#include <QtTest/QtTest>


class Q3CalcTest : public QObject
{
    Q_OBJECT
public:
    Q3CalcTest();
    ~Q3CalcTest();

private slots:
    void incompleteCholeskyTest();
};

#endif // Q3CALCTEST_H
