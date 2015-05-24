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

QVector2D Q3BoundaryIn::velocity(Q3SceletonItem *item, QPointF a, QPointF b)
{
    QPointF point = 0.5 * (a + b);
    return QVector2D(velocityXExp_.getValue(point.x(), point.y()),
                     velocityYExp_.getValue(point.x(), point.y()));

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
