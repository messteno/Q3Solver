#ifndef ITEMLISTMODEL_H
#define ITEMLISTMODEL_H

#include <QAbstractTableModel>
#include "qmeshitem.h"

class ItemListModel : public QAbstractTableModel
{
    Q_OBJECT
public:
    explicit ItemListModel(QObject *parent = 0);
    virtual int rowCount(const QModelIndex &parent) const;
    virtual int columnCount(const QModelIndex &parent) const;
    QVariant data(const QModelIndex &index, int role) const;
    QVariant headerData(int section, Qt::Orientation orientation, int role) const;
    virtual void addItem(QMeshItem *item);
    void insertRow(int row, const QModelIndex &parent);

signals:

public slots:

private:
    int rows_;
    QVector<QMeshItem *> items_;
};

#endif // ITEMLISTMODEL_H
