#ifndef Q3VECTOR_H
#define Q3VECTOR_H

#include <QVector>

class Q3Vector : public QVector<qreal>
{
public:
    Q3Vector();
    Q3Vector(int size);
    Q3Vector(const QVector &vec);

    Q3Vector operator+(const Q3Vector &vec) const;
    Q3Vector operator-(const Q3Vector &vec) const;
    void operator+=(const Q3Vector &vec);
    qreal norm() const;

    static qreal dot(const Q3Vector &v1, const Q3Vector &v2);
    friend Q3Vector operator* (qreal coef, const Q3Vector &vec);
};

Q3Vector operator* (qreal coef, const Q3Vector &vec);

#endif // Q3VECTOR_H
