#include "q3boundaryin.h"
#include "ui_q3boundaryin.h"

Q3BoundaryIn::Q3BoundaryIn(QWidget *parent) :
    Q3BoundaryType(Q3BoundaryType::InBoundary, parent),
    ui(new Ui::Q3BoundaryIn)
{
    ui->setupUi(this);
}

Q3BoundaryIn::~Q3BoundaryIn()
{
    delete ui;
}

QVector2D Q3BoundaryIn::velocity(Q3SceletonItem *item, QPointF point)
{
    return QVector2D(ui->vxEdit->text().toDouble(),
                     ui->vyEdit->text().toDouble());
}
