#ifndef Q3SCELETONITEMFORM_H
#define Q3SCELETONITEMFORM_H

#include <QWidget>
#include "q3itemvisitor.h"
#include "q3sceletonitem.h"

class Q3SceletonItemForm : public QWidget, public Q3ItemVisitor
{
    Q_OBJECT
public:
    explicit Q3SceletonItemForm(QWidget *parent = 0);
    ~Q3SceletonItemForm();

    bool visit(Q3Point *, Q3Point *) { return false; }
    bool visit(Q3Point *, Q3PointConnection *) { return false; }
    bool visit(Q3Point *, Q3Circle *) { return false; }
    bool visit(Q3PointConnection *, Q3PointConnection *) { return false; }
    bool visit(Q3PointConnection *, Q3Circle *) { return false; }
    bool visit(Q3Circle *, Q3Circle *) { return false; }

    virtual bool visit(Q3Point *) { return false; }
    virtual bool visit(Q3PointConnection *) { return false; }
    virtual bool visit(Q3Circle *) { return false; }

    virtual Q3SceletonItem* createItem() = 0;
    virtual void clear() = 0;

signals:

public slots:
};

#endif // Q3SCELETONITEMFORM_H
