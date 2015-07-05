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

QVector2D Q3BoundaryFixedVelocity::velocity(Q3SceletonItem *item,
                                            const QPointF &a, const QPointF &b,
                                            qreal time)
{
    Q_UNUSED(time);
    Q_ASSERT(item);

    switch (item->type())
    {
        case Q3SceletonItem::Circle:
        case Q3SceletonItem::PointConnection:
        {
            QVector2D tangentVector = QVector2D(b - a).normalized();
            return ui->vEdit->text().toDouble() * tangentVector;
        }
        default:
            break;
    }
    return QVector2D(0, 0);
}

void Q3BoundaryFixedVelocity::setVelocityText(const QString &vText)
{
    ui->vEdit->setText(vText);
}
