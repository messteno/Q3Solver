#include "q3point.h"
#include "q3pointform.h"
#include "ui_q3pointform.h"

Q3PointForm::Q3PointForm(QWidget *parent) :
    Q3SceletonItemForm(parent),
    ui(new Ui::Q3PointForm)
{
    ui->setupUi(this);
    ui->xEdit->setValidator(new QDoubleValidator(-1000000, 1000000, 8, this));
    ui->yEdit->setValidator(new QDoubleValidator(-1000000, 1000000, 8, this));
}

Q3PointForm::~Q3PointForm()
{
    delete ui;
}

bool Q3PointForm::getXY(qreal &x, qreal &y)
{
    if (ui->xEdit->text().isEmpty() || ui->yEdit->text().isEmpty())
        return false;

    x = ui->xEdit->text().toDouble();
    y = ui->yEdit->text().toDouble();

    return true;
}

void Q3PointForm::setXY(qreal x, qreal y)
{
    ui->xEdit->setText(QString::number(x));
    ui->yEdit->setText(QString::number(y));
}

Q3SceletonItem *Q3PointForm::createItem()
{
    qreal x, y;
    if (!getXY(x, y))
        return NULL;

    Q3Point *point = new Q3Point(QPointF(x, y));
    return point;
}

bool Q3PointForm::visit(Q3Point *point)
{
    qreal x, y;
    if (!getXY(x, y))
    {
        setXY(point->x(), point->y());
        return false;
    }

    point->setX(x);
    point->setY(y);

    return true;
}

void Q3PointForm::clear()
{
    ui->xEdit->clear();
    ui->yEdit->clear();
}
