#ifndef POINTADDITEMWIDGET_H
#define POINTADDITEMWIDGET_H

#include <QWidget>
#include "additemwidget.h"

namespace Ui {
class PointAddItemWidget;
}

class PointAddItemWidget : public AddItemWidget
{
    Q_OBJECT

public:
    PointAddItemWidget(AddItemDirector *director, QWidget *parent = 0);
    virtual void expand();
    virtual void shrink();
    virtual QMeshItem* getItem();

private:
    Ui::PointAddItemWidget *ui;
};

#endif // POINTADDITEMWIDGET_H
