#ifndef Q3ANI2DTEST_H
#define Q3ANI2DTEST_H

#include <QtTest/QtTest>

class Q3Ani2DTest : public QObject
{
    Q_OBJECT
public:
    Q3Ani2DTest();
    ~Q3Ani2DTest();
private slots:
    void ani2DCreateMesh();
};

#endif // Q3ANI2DTEST_H
