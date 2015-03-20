#ifndef Q3POINTFORM_H
#define Q3POINTFORM_H

#include "q3sceletonitemform.h"

namespace Ui {
class Q3PointForm;
}

class Q3PointForm : public Q3SceletonItemForm
{
    Q_OBJECT

public:
    explicit Q3PointForm(QWidget *parent = 0);
    ~Q3PointForm();

private:
    Ui::Q3PointForm *ui;
    bool getXY(qreal &x, qreal &y);
    void setXY(qreal x, qreal y);

public:
    Q3SceletonItem *createItem();
    bool visit(Q3Point *point);
    void clear();
};

#endif // Q3POINTFORM_H
