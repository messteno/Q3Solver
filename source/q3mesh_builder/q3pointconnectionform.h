#ifndef Q3POINTCONNECTIONFORM_H
#define Q3POINTCONNECTIONFORM_H

#include <QWidget>
#include <QMap>

#include "q3point.h"
#include "q3sceletonitemform.h"

namespace Ui {
class Q3PointConnectionForm;
}

class Q3PointConnectionForm : public Q3SceletonItemForm
{
    Q_OBJECT

public:
    explicit Q3PointConnectionForm(QWidget *parent = 0);
    ~Q3PointConnectionForm();

    Q3SceletonItem *createItem();
    bool visit(Q3Point *point);
    bool visit(Q3PointConnection *conn);
    void clear();

private:
    Ui::Q3PointConnectionForm *ui;
    QMap<int, Q3Point *> pointMap_;

    bool getPoints(Q3Point *&a, Q3Point *&b);
    void setPoints(Q3Point *a, Q3Point *b);
};

#endif // Q3POINTCONNECTIONFORM_H
