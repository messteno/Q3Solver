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
    PointAddItemWidget(AddItemDirector *director, const QString &addButtonText);
    virtual void meshPlotClicked(QMeshPlot *meshPlot);
    virtual bool addItem();
    virtual void clear();

private:
    Ui::PointAddItemWidget *ui;
};

#endif // POINTADDITEMWIDGET_H
