#include <qmath.h>
#include "q3vector.h"

Q3Vector::Q3Vector() :
    QVector()
{

}

Q3Vector::Q3Vector(int size) :
    QVector<qreal>(size)
{

}

Q3Vector::Q3Vector(const QVector &vec) :
    QVector(vec)
{

}

Q3Vector Q3Vector::operator+(const Q3Vector &vec) const
{
    Q_ASSERT(size() == vec.size());
    Q3Vector sum(size());

    for (int i = 0; i < size(); ++i)
        sum[i] = at(i) + vec.at(i);
    return sum;
}

Q3Vector Q3Vector::operator-(const Q3Vector &vec) const
{
    Q_ASSERT(size() == vec.size());
    Q3Vector diff(size());

    for (int i = 0; i < size(); ++i)
        diff[i] = at(i) - vec.at(i);
    return diff;
}

void Q3Vector::operator+=(const Q3Vector &vec)
{
    Q_ASSERT(size() == vec.size());

    for (int i = 0; i < size(); ++i)
        data()[i] += vec.at(i);
}

qreal Q3Vector::norm() const
{
    qreal sum = 0;
    for (int i = 0; i < size(); ++i)
        sum += at(i) * at(i);
    return sqrt(sum);
}

qreal Q3Vector::dot(const Q3Vector &v1, const Q3Vector &v2)
{
    Q_ASSERT(v1.size() == v2.size());

    qreal sum = 0;
    for (int i = 0; i < v1.size(); ++i)
        sum += v1[i] * v2[i];
    return sum;
}

Q3Vector operator*(qreal coef, const Q3Vector &vec)
{
    Q3Vector cvec(vec.size());
    for (int i = 0; i < vec.size(); ++i)
        cvec[i] = coef * vec.at(i);
    return cvec;
}
