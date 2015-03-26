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

    bool visit(Q3Point *point1, Q3Point *point2) { return false; }
    bool visit(Q3Point *point, Q3PointConnection *conn) { return false; }
    bool visit(Q3Point *point, Q3Circle *circle) { return false; }
    bool visit(Q3PointConnection *conn1, Q3PointConnection *conn2) { return false; }
    bool visit(Q3PointConnection *conn, Q3Circle *circle) { return false; }
    bool visit(Q3Circle *circle1, Q3Circle *circle2) { return false; }

    virtual bool visit(Q3Point *point) { return false; }
    virtual bool visit(Q3PointConnection *conn) { return false; }
    virtual bool visit(Q3Circle *circle) { return false; }

    virtual Q3SceletonItem* createItem() = 0;
    virtual void clear() = 0;

signals:

public slots:
};

#endif // Q3SCELETONITEMFORM_H
