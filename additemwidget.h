#ifndef ADDITEMWIDGET_H
#define ADDITEMWIDGET_H

#include <QWidget>
#include <QPushButton>
#include "qmeshplot.h"
#include "qmeshitem.h"

class AddItemDirector;

class AddItemWidget : public QWidget
{
    Q_OBJECT
public:
    explicit AddItemWidget(AddItemDirector *director, const QString &addButtonText);
    virtual QPushButton* getAddButton();
    virtual void meshPlotClicked(QMeshPlot *meshPlot);
    virtual bool addItem() = 0;
    virtual void clear();
protected:
    AddItemDirector *director_;
    bool expanded_;

protected slots:
    virtual void selected();

private:
    QPushButton addButton_;
};

#endif // ADDITEMWIDGET_H
