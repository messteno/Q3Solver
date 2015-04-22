#include "q3circleform.h"
#include "ui_q3circleform.h"

Q3CircleForm::Q3CircleForm(QWidget *parent) :
    Q3SceletonItemForm(parent),
    ui(new Ui::Q3CircleForm)
{
    ui->setupUi(this);
    ui->xEdit->setValidator(new QDoubleValidator(-1000000, 1000000, 8, this));
    ui->yEdit->setValidator(new QDoubleValidator(-1000000, 1000000, 8, this));
    ui->rEdit->setValidator(new QDoubleValidator(-1000000, 1000000, 8, this));
}

Q3CircleForm::~Q3CircleForm()
{
    delete ui;
}

bool Q3CircleForm::getXYR(qreal &x, qreal &y, qreal &r)
{
    if (ui->xEdit->text().isEmpty()
        || ui->yEdit->text().isEmpty()
        || ui->rEdit->text().isEmpty())
    {
        return false;
    }

    x = ui->xEdit->text().toDouble();
    y = ui->yEdit->text().toDouble();
    r = ui->rEdit->text().toDouble();

    return true;
}

void Q3CircleForm::setXYR(qreal x, qreal y, qreal r)
{
    ui->xEdit->setText(QString::number(x));
    ui->yEdit->setText(QString::number(y));
    ui->rEdit->setText(QString::number(r));
}

Q3SceletonItem *Q3CircleForm::createItem()
{
    qreal x, y, r;
    if (!getXYR(x, y, r))
        return NULL;

    Q3Circle *circle = new Q3Circle(QPointF(x, y), r);
    return circle;
}

bool Q3CircleForm::visit(Q3Circle *circle)
{
    qreal x, y, r;
    if (!getXYR(x, y, r))
    {
        setXYR(circle->center().x(), circle->center().y(), circle->radius());
        return false;
    }

    circle->setCenter(QPointF(x, y));
    circle->setRadius(r);

    return true;
}

void Q3CircleForm::clear()
{
    ui->xEdit->clear();
    ui->yEdit->clear();
    ui->rEdit->clear();
}
