#include <QComboBox>
#include <QApplication>
#include <QDebug>

#include "q3sceleton.h"
#include "q3mesh.h"
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
        Q3Mesh::BoundaryType boundaryType = \
                static_cast<Q3Mesh::BoundaryType>(value);
        if (boundaryType == Q3Mesh::CannotBeBoundary)
            return QItemDelegate::createEditor(parent, option, index);

        QComboBox* comboBox = new QComboBox(parent);
        comboBox->addItem(Q3Mesh::boundaryTypeToString(Q3Mesh::NotBoundary),
                          QVariant(static_cast<uint>(Q3Mesh::NotBoundary)));
        comboBox->addItem(Q3Mesh::boundaryTypeToString(Q3Mesh::InBoundary),
                          QVariant(static_cast<uint>(Q3Mesh::InBoundary)));
        comboBox->addItem(Q3Mesh::boundaryTypeToString(Q3Mesh::OutBoundary),
                          QVariant(static_cast<uint>(Q3Mesh::OutBoundary)));
        comboBox->addItem(Q3Mesh::boundaryTypeToString(Q3Mesh::MoveBoundary),
                          QVariant(static_cast<uint>(Q3Mesh::MoveBoundary)));
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
        Q3Mesh::BoundaryType boundaryType = \
                static_cast<Q3Mesh::BoundaryType>(value);
        if (boundaryType != Q3Mesh::CannotBeBoundary)
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
        Q3Mesh::BoundaryType boundaryType = \
                static_cast<Q3Mesh::BoundaryType>(value);
        if (boundaryType == Q3Mesh::CannotBeBoundary)
            return;

        QStyleOptionComboBox comboBoxOption;
        comboBoxOption.rect = option.rect;
        comboBoxOption.state = QStyle::State_Active; // | QStyle::State_Enabled;
        comboBoxOption.currentText = Q3Mesh::boundaryTypeToString(boundaryType);
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
