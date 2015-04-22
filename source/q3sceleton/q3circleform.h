#ifndef Q3CIRCLEFORM_H
#define Q3CIRCLEFORM_H

#include "q3sceletonitemform.h"

namespace Ui {
class Q3CircleForm;
}

class Q3CircleForm : public Q3SceletonItemForm
{
    Q_OBJECT

public:
    explicit Q3CircleForm(QWidget *parent = 0);
    ~Q3CircleForm();

private:
    Ui::Q3CircleForm *ui;
    bool getXYR(qreal &x, qreal &y, qreal &r);
    void setXYR(qreal x, qreal y, qreal r);

public:
    Q3SceletonItem *createItem();
    bool visit(Q3Circle *circle);
    void clear();
};

#endif // Q3CIRCLEFORM_H
