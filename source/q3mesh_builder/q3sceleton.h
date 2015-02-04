#ifndef Q3SCELETON_H
#define Q3SCELETON_H

#include <QWidget>
#include <QList>

#include "q3sceletonitem.h"
#include "q3painter.h"
#include "q3plotdrawable.h"

class Q3Plot;

class Q3Sceleton : public Q3PlotDrawable
{
    Q_OBJECT

private:
    QList<Q3SceletonItem *> items_;
public:
    Q3Sceleton(QWidget *parent);
    virtual ~Q3Sceleton();
    Q3SceletonItem* itemAt(const QPointF &pos, qreal radius,
                           Q3SceletonItem::Type type = Q3SceletonItem::Base) const;
    Q3SceletonItem *itemToResizeAt(const QPointF &pos, qreal radius) const;
    void addItem(Q3SceletonItem *item);
    void removeItem(Q3SceletonItem *item);
    void removeSelectedItems();
    void draw(Q3Painter &painter) const;
};

#endif // Q3SCELETON_H
