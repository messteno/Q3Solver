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

QVector2D Q3BoundaryFixedVelocity::velocity(Q3SceletonItem *item, QPointF point)
{
    switch (item->type())
    {
        case Q3SceletonItem::PointConnection:
        {
            Q3PointConnection *conn = dynamic_cast<Q3PointConnection *>(item);
            Q_ASSERT(conn);
            QVector2D tangentVector = QVector2D(conn->b()->point()
                                                - conn->a()->point());
            return ui->vEdit->text().toDouble() * tangentVector;
        }
        default:
            break;
    }
    return QVector2D(0, 0);
}
