#include "q3pointconnectionform.h"
#include "ui_q3pointconnectionform.h"

Q3PointConnectionForm::Q3PointConnectionForm(QWidget *parent) :
    Q3SceletonItemForm(parent),
    ui(new Ui::Q3PointConnectionForm)
{
    ui->setupUi(this);
    ui->p1ComboBox->addItem("Не выбрана", -1);
    ui->p2ComboBox->addItem("Не выбрана", -1);
}

Q3PointConnectionForm::~Q3PointConnectionForm()
{
    delete ui;
}

Q3SceletonItem *Q3PointConnectionForm::createItem()
{
    Q3Point *a, *b;
    if (!getPoints(a, b))
        return NULL;

    Q3PointConnection *conn = new Q3PointConnection(a, b);
    return conn;
}

bool Q3PointConnectionForm::visit(Q3Point *point)
{
    int index = pointMap_.size();
    pointMap_[index] = point;

    ui->p1ComboBox->addItem(point->toString(), index);
    ui->p2ComboBox->addItem(point->toString(), index);
    return true;
}

bool Q3PointConnectionForm::visit(Q3PointConnection *conn)
{
    Q3Point *a, *b;
    if (!getPoints(a, b))
    {
        setPoints(conn->a(), conn->b());
        return false;
    }

    conn->setA(a);
    conn->setB(b);
    return true;
}

void Q3PointConnectionForm::clear()
{
    ui->p1ComboBox->setCurrentIndex(0);
    ui->p2ComboBox->setCurrentIndex(0);
}

bool Q3PointConnectionForm::getPoints(Q3Point* &a, Q3Point* &b)
{
    int indexPointA = ui->p1ComboBox->currentData().toInt();
    int indexPointB = ui->p2ComboBox->currentData().toInt();

    if (indexPointA == -1 || indexPointB == -1 || indexPointA == indexPointB)
        return false;

    if (!pointMap_.contains(indexPointA) || !pointMap_.contains(indexPointB))
        return false;

    a = pointMap_[indexPointA];
    b = pointMap_[indexPointB];

    return true;
}

void Q3PointConnectionForm::setPoints(Q3Point *a, Q3Point *b)
{
    int indexPointA = ui->p1ComboBox->findData(pointMap_.key(a));
    int indexPointB = ui->p2ComboBox->findData(pointMap_.key(b));

    ui->p1ComboBox->setCurrentIndex(indexPointA);
    ui->p2ComboBox->setCurrentIndex(indexPointB);
}
