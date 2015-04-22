#include "q3boundaryout.h"
#include "ui_q3boundaryout.h"

Q3BoundaryOut::Q3BoundaryOut(QWidget *parent) :
    Q3BoundaryType(Q3BoundaryType::OutBoundary, parent),
    ui(new Ui::Q3BoundaryOut)
{
    ui->setupUi(this);
}

Q3BoundaryOut::~Q3BoundaryOut()
{
    delete ui;
}
