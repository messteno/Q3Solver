#ifndef Q3BOUNDARYOUT_H
#define Q3BOUNDARYOUT_H

#include <QWidget>

#include "q3boundarytype.h"

namespace Ui {
class Q3BoundaryOut;
}

class Q3BoundaryOut : public Q3BoundaryType
{
    Q_OBJECT

public:
    explicit Q3BoundaryOut(QWidget *parent = 0);
    ~Q3BoundaryOut();

private:
    Ui::Q3BoundaryOut *ui;
};

#endif // Q3BOUNDARYOUT_H
