#ifndef SEGMENTADDITEMWIDGET_H
#define SEGMENTADDITEMWIDGET_H

#include <QWidget>
#include "additemwidget.h"

class QMeshItemPoint;

namespace Ui {
class LineAddItemWidget;
}

class SegmentAddItemWidget : public AddItemWidget
{
    Q_OBJECT

public:
    SegmentAddItemWidget(AddItemDirector *director, const QString &addButtonText);
    virtual void meshPlotClicked(QMeshPlot *meshPlot);
    virtual bool addItem();
    virtual void clear();

private:
    Ui::LineAddItemWidget *ui;
    QMeshItemPoint *aPoint_;
    QMeshItemPoint *bPoint_;
};

#endif // LINEADDITEMWIDGET_H
