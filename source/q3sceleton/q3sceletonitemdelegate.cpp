#include <QComboBox>
#include <QApplication>
#include <QDebug>

#include "q3sceleton.h"
#include "q3sceletonitemdelegate.h"

Q3SceletonItemDelegate::Q3SceletonItemDelegate(QObject *parent) :
    QItemDelegate(parent)
{

}

Q3SceletonItemDelegate::~Q3SceletonItemDelegate()
{

}

QWidget *Q3SceletonItemDelegate::createEditor(QWidget *parent,
                                              const QStyleOptionViewItem &option,
                                              const QModelIndex &index) const
{
    if (index.column() == Q3Sceleton::ColumnBoundary)
    {
        uint value = index.model()->data(index, Qt::DisplayRole).toUInt();
        Q3SceletonItem::BoundaryType boundaryType = \
                static_cast<Q3SceletonItem::BoundaryType>(value);
        if (boundaryType == Q3SceletonItem::CannotBeBoundary)
            return QItemDelegate::createEditor(parent, option, index);

        QComboBox* comboBox = new QComboBox(parent);
        comboBox->addItem(Q3SceletonItem::boundaryTypeToString(Q3SceletonItem::NotBoundary),
                          QVariant(static_cast<uint>(Q3SceletonItem::NotBoundary)));
        comboBox->addItem(Q3SceletonItem::boundaryTypeToString(Q3SceletonItem::InBoundary),
                          QVariant(static_cast<uint>(Q3SceletonItem::InBoundary)));
        comboBox->addItem(Q3SceletonItem::boundaryTypeToString(Q3SceletonItem::OutBoundary),
                          QVariant(static_cast<uint>(Q3SceletonItem::OutBoundary)));
        comboBox->addItem(Q3SceletonItem::boundaryTypeToString(Q3SceletonItem::MoveBoundary),
                          QVariant(static_cast<uint>(Q3SceletonItem::MoveBoundary)));
        return comboBox;
    }
    else
    {
        return QItemDelegate::createEditor(parent, option, index);
    }
}

void Q3SceletonItemDelegate::setEditorData(QWidget *editor,
                                           const QModelIndex &index) const
{
    if (index.column() == Q3Sceleton::ColumnBoundary)
    {
        QComboBox *comboBox = qobject_cast<QComboBox*>(editor);
        uint value = index.model()->data(index, Qt::DisplayRole).toUInt();
        Q3SceletonItem::BoundaryType boundaryType = \
                static_cast<Q3SceletonItem::BoundaryType>(value);
        if (boundaryType != Q3SceletonItem::CannotBeBoundary)
        {
            comboBox->setCurrentIndex(comboBox->findData(QVariant(value)));
            comboBox->showPopup();
        }
        else
            QItemDelegate::setEditorData(editor, index);
    }
    else
    {
        QItemDelegate::setEditorData(editor, index);
    }
}

void Q3SceletonItemDelegate::setModelData(QWidget *editor,
                                          QAbstractItemModel *model,
                                          const QModelIndex &index) const
{
    QComboBox *comboBox = qobject_cast<QComboBox*>(editor);
    if (comboBox)
        model->setData(index, comboBox->currentData(), Qt::EditRole);
}

void Q3SceletonItemDelegate::paint(QPainter *painter,
                                   const QStyleOptionViewItem &option,
                                   const QModelIndex &index) const
{
    if (index.column() == Q3Sceleton::ColumnBoundary)
    {
        uint value = index.model()->data(index, Qt::DisplayRole).toUInt();
        Q3SceletonItem::BoundaryType boundaryType = \
                static_cast<Q3SceletonItem::BoundaryType>(value);
        if (boundaryType == Q3SceletonItem::CannotBeBoundary)
            return;

        QStyleOptionComboBox comboBoxOption;
        comboBoxOption.rect = option.rect;
        comboBoxOption.state = QStyle::State_Active; // | QStyle::State_Enabled;
        comboBoxOption.currentText = Q3SceletonItem::boundaryTypeToString(boundaryType);
        QApplication::style()->drawComplexControl(QStyle::CC_ComboBox,
                                                  &comboBoxOption, painter);
        QApplication::style()->drawControl(QStyle::CE_ComboBoxLabel,
                                           &comboBoxOption, painter);
    }
    else
    {
        QItemDelegate::paint(painter, option, index);
    }
}
