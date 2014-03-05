#ifndef LINEADDITEMWIDGET_H
#define LINEADDITEMWIDGET_H

#include <QWidget>
#include "additemwidget.h"

namespace Ui {
class LineAddItemWidget;
}

class LineAddItemWidget : public AddItemWidget
{
    Q_OBJECT

public:
    LineAddItemWidget(AddItemDirector *director, QWidget *parent = 0);
    virtual void expand();
    virtual void shrink();
    virtual QMeshItem* getItem();

private:
    Ui::LineAddItemWidget *ui;
};

#endif // LINEADDITEMWIDGET_H
