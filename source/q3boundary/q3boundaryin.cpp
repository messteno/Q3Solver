#include <QDebug>

#include "q3boundaryin.h"
#include "ui_q3boundaryin.h"

Q3BoundaryIn::Q3BoundaryIn(QWidget *parent) :
    Q3BoundaryType(Q3BoundaryType::InBoundary, parent),
    ui(new Ui::Q3BoundaryIn)
{
    ui->setupUi(this);
    save();
}

Q3BoundaryIn::~Q3BoundaryIn()
{
    delete ui;
}

QVector2D Q3BoundaryIn::velocity(Q3SceletonItem *item,
                                 const QPointF &a, const QPointF &b, qreal time)
{
    Q_UNUSED(item);

    QPointF point = 0.5 * (a + b);
    QVector2D velocity(velocityXExp_.getValue(point.x(), point.y(), time),
                       velocityYExp_.getValue(point.x(), point.y(), time));
    return velocity;

    //return QVector2D(ui->vxEdit->text().toDouble(),
    //                 ui->vyEdit->text().toDouble());
}

void Q3BoundaryIn::setVelocityText(const QString &vXText, const QString &vYText)
{
    ui->vxEdit->setText(vXText);
    ui->vyEdit->setText(vYText);
    save();
}

void Q3BoundaryIn::save()
{
    std::string xExprStr = ui->vxEdit->text().toStdString();
    std::string yExprStr = ui->vyEdit->text().toStdString();

    if (!velocityXExp_.setString(xExprStr))
        QMessageBox::warning(NULL, tr("Q3Solver"),
                tr("Не удалось обработать выражение для скорости по оси X."));

    if (!velocityYExp_.setString(yExprStr))
        QMessageBox::warning(NULL, tr("Q3Solver"),
                tr("Не удалось обработать выражение для скорости по оси Y."));
}
