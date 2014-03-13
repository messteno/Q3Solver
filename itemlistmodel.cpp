#include <QDebug>
#include <QComboBox>
#include "itemlistmodel.h"

ItemListModel::ItemListModel(QObject *parent) :
    QAbstractTableModel(parent)
{
    rows_ = 1;
}

int ItemListModel::rowCount(const QModelIndex &parent) const
{
    return items_.size();
}

int ItemListModel::columnCount(const QModelIndex &parent) const
{
    return 3;
}

QVariant ItemListModel::data(const QModelIndex &index, int role) const
{
    if (role == Qt::DisplayRole)
    {
        if (index.row() >= items_.count())
            return QVariant();
        QMeshItem *item = items_[index.row()];
        QString text = "";
        switch (index.column())
        {
        case 0:
            text = item->getName();
            break;
        case 1:
            text = item->getValueText();
            break;
        case 2:
            text = item->getTypeText();
            break;
        }
        return text;
    }
    return QVariant();
}

QVariant ItemListModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    if (role == Qt::DisplayRole)
    {
        if (orientation == Qt::Horizontal)
        {
            if (section == 0)
                return QString(trUtf8("Объект"));
            else if (section == 1)
                return QString(trUtf8("Данные"));
            else if (section == 2)
                return QString(trUtf8("Тип"));
            else
                return QVariant();
        }
        else
        {
            return QString::number(section + 1);
        }
    }
    return QVariant();
}

void ItemListModel::addItem(QMeshItem *item)
{
    if (item)
    {
        beginInsertRows(QModelIndex(), items_.count(), items_.count());
        items_.push_back(item);
        endInsertRows();

//        QModelIndex top = createIndex(people.count() - 1, 0, 0);
//        QModelIndex bottom = createIndex(people.count() - 1, 3, 0);
    }
}
