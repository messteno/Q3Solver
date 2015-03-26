#ifndef Q3SCELETON_H
#define Q3SCELETON_H

#include <QWidget>
#include <QList>
#include <QAbstractTableModel>

#include "q3sceletonitem.h"
#include "q3painter.h"
#include "q3plotdrawable.h"

class Q3Plot;

class Q3Sceleton : public QAbstractTableModel, public Q3PlotDrawable
{
    Q_OBJECT

public:
    enum ColumnType
    {
        ColumnElement,
        ColumnParameters,
        ColumnBoundary,
    };

    Q3Sceleton(QWidget *parent);
    virtual ~Q3Sceleton();
    Q3SceletonItem* itemAt(const QPointF &pos, qreal radius,
                           Q3SceletonItem::Type type = Q3SceletonItem::Base) const;
    Q3SceletonItem *itemToResizeAt(const QPointF &pos, qreal radius) const;
    void addItem(Q3SceletonItem *item);
    void removeSelectedItems();
    void draw(Q3Painter &painter) const;
    bool prepare();

    bool isActive() const;
    void setActive(bool active);

    QList<Q3SceletonItem *>& items();
    QList<Q3SceletonItem *>& outerBoundary();
    QList<QList<Q3SceletonItem *> >& innerBoundaries();
    QList<Q3SceletonItem *>& innerElements();

signals:
    void createMeshProgress(int value);

public slots:
    void itemsUpdated();

private:
    static const int tableColumns_;

    QList<Q3SceletonItem *> items_;
    QList<Q3SceletonItem *> outerBoundary_;
    QList<QList<Q3SceletonItem *> > innerBoundaries_;
    QList<Q3SceletonItem *> innerElements_;

    bool active_;

    int rowCount(const QModelIndex &parent) const;
    int columnCount(const QModelIndex &parent) const;
    QVariant data(const QModelIndex &index, int role) const;
    bool setData(const QModelIndex &index, const QVariant &value, int role);
    QVariant headerData(int section, Qt::Orientation orientation, int role) const;
    Qt::ItemFlags flags(const QModelIndex &index) const;
};

#endif // Q3SCELETON_H
