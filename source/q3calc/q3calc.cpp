#include <QDebug>

#include "q3calc.h"

Q3Calc::Q3Calc(Q3Mesh *mesh, QObject *parent) :
    QThread(parent),
    mesh_(mesh)
{
}

Q3Calc::~Q3Calc()
{
    abort_ = true;
    wait();
}

void Q3Calc::run()
{
    abort_ = false;
    while(!abort_)
    {
    }
}

void Q3Calc::abort()
{
    abort_ = true;
}
