#include "q3boundarynoslip.h"
#include "ui_q3boundarynoslip.h"

Q3BoundaryNoSlip::Q3BoundaryNoSlip(QWidget *parent) :
    Q3BoundaryType(Q3BoundaryType::NoSlipBoundary, parent),
    ui(new Ui::Q3BoundaryNoSlip)
{
    ui->setupUi(this);
}

Q3BoundaryNoSlip::~Q3BoundaryNoSlip()
{
    delete ui;
}
