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
    LineAddItemWidget(AddItemDirector *director, const QString &addButtonText);
    virtual QMeshItem* getItem();

private:
    Ui::LineAddItemWidget *ui;
};

#endif // LINEADDITEMWIDGET_H
