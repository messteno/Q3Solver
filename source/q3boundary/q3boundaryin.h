#ifndef Q3BOUNDARYIN_H
#define Q3BOUNDARYIN_H

#include "q3boundarytype.h"

namespace Ui {
class Q3BoundaryIn;
}

class Q3BoundaryIn : public Q3BoundaryType
{
    Q_OBJECT

public:
    explicit Q3BoundaryIn(QWidget *parent = 0);
    ~Q3BoundaryIn();

private:
    Ui::Q3BoundaryIn *ui;
};

#endif // Q3BOUNDARYIN_H
