#ifndef ADDITEMWIDGET_H
#define ADDITEMWIDGET_H

#include <QWidget>
#include "qmeshitem.h"

class AddItemDirector;

class AddItemWidget : public QWidget
{
    Q_OBJECT
public:
    explicit AddItemWidget(AddItemDirector *director, QWidget *parent = 0);
    virtual void shrink();
    virtual void expand();
    virtual QMeshItem* getItem() = 0;
protected:
    bool expanded_;

protected slots:
    virtual void selected();

private:
    AddItemDirector *director_;
};

#endif // ADDITEMWIDGET_H
