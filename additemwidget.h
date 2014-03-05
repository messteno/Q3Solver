#ifndef ADDITEMWIDGET_H
#define ADDITEMWIDGET_H

#include <QWidget>
#include <QPushButton>
#include "qmeshitem.h"

class AddItemDirector;

class AddItemWidget : public QWidget
{
    Q_OBJECT
public:
    explicit AddItemWidget(AddItemDirector *director, const QString &addButtonText);
    virtual QMeshItem* getItem() = 0;
    virtual QPushButton* getAddButton();
protected:
    bool expanded_;

protected slots:
    virtual void selected();

private:
    AddItemDirector *director_;
    QPushButton addButton_;
};

#endif // ADDITEMWIDGET_H
