#ifndef Q3BOUNDARYNOSLIP_H
#define Q3BOUNDARYNOSLIP_H

#include <QWidget>

#include "q3boundarytype.h"

namespace Ui {
class Q3BoundaryNoSlip;
}

class Q3BoundaryNoSlip : public Q3BoundaryType
{
    Q_OBJECT

public:
    explicit Q3BoundaryNoSlip(QWidget *parent = 0);
    ~Q3BoundaryNoSlip();

private:
    Ui::Q3BoundaryNoSlip *ui;
};

#endif // Q3BOUNDARYNOSLIP_H
