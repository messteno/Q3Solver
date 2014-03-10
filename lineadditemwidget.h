#ifndef LINEADDITEMWIDGET_H
#define LINEADDITEMWIDGET_H

#include <QWidget>
#include "additemwidget.h"

class QMeshItemPoint;

namespace Ui {
class LineAddItemWidget;
}

class LineAddItemWidget : public AddItemWidget
{
    Q_OBJECT

public:
    LineAddItemWidget(AddItemDirector *director, const QString &addButtonText);
    virtual QMeshItem* getItem();
    virtual void meshPlotClicked(QMeshPlot *meshPlot);
    virtual void clear();

private:
    Ui::LineAddItemWidget *ui;
    QMeshItemPoint *aPoint_;
    QMeshItemPoint *bPoint_;
};

#endif // LINEADDITEMWIDGET_H
