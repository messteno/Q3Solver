#include "q3boundaryfixedvelocity.h"
#include "ui_q3boundaryfixedvelocity.h"

Q3BoundaryFixedVelocity::Q3BoundaryFixedVelocity(QWidget *parent) :
    Q3BoundaryType(Q3BoundaryType::FixedVelocity, parent),
    ui(new Ui::Q3BoundaryFixedVelocity)
{
    ui->setupUi(this);
}

Q3BoundaryFixedVelocity::~Q3BoundaryFixedVelocity()
{
    delete ui;
}
