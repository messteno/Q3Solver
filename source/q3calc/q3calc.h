#ifndef Q3CALC_H
#define Q3CALC_H

#include <QThread>

#include "q3mesh.h"

class Q3Calc : public QThread
{
    Q_OBJECT

public:
    Q3Calc(Q3Mesh *mesh, QObject *parent = 0);
    ~Q3Calc();

    void abort();

protected:
    void run() Q_DECL_OVERRIDE;
    Q3Mesh *mesh_;
    bool abort_;
};

#endif // Q3CALC_H

