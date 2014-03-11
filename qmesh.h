#ifndef QMESH_H
#define QMESH_H

#include <QWidget>
#include "additemdirector.h"
#include "qmeshitem.h"
#include "itemlistmodel.h"

namespace Ui {
class QMesh;
}

class QMesh : public QWidget
{
    Q_OBJECT

public:
    explicit QMesh(QWidget *parent = 0);
    ~QMesh();
    const QList<QMeshItem *> &getItems();
    void addItem(QMeshItem *item);

private slots:

    void on_addElementButton_clicked();
    void on_cancelElementButton_clicked();

private:
    Ui::QMesh *ui;
    AddItemDirector *addItemDirector_;
    QMeshPlot *meshPlot_;
    QList<QMeshItem *> items_;
    ItemListModel *itemListModel_;
};

#endif // QMESH_H
