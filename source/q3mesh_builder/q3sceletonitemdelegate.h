#ifndef Q3SCELETONITEMDELEGATE_H
#define Q3SCELETONITEMDELEGATE_H

#include <QItemDelegate>

class Q3SceletonItemDelegate : public QItemDelegate
{
    Q_OBJECT

public:
    Q3SceletonItemDelegate(QObject *parent = 0);
    ~Q3SceletonItemDelegate();

    QWidget* createEditor(QWidget *parent, const QStyleOptionViewItem &option,
                          const QModelIndex &index) const;
    void setEditorData(QWidget *editor, const QModelIndex &index) const;
    void setModelData(QWidget *editor, QAbstractItemModel *model, const QModelIndex &index) const;
    void paint(QPainter *painter, const QStyleOptionViewItem &option,
               const QModelIndex &index) const;
};

#endif // Q3SCELETONITEMDELEGATE_H
