#ifndef QMESH_H
#define QMESH_H

#include <QWidget>
#include "additemdirector.h"
#include "qmeshitem.h"
#include "itemlistmodel.h"
#include "meshgenerator.h"

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

    void on_createMeshButton_clicked();

private:
    Ui::QMesh *ui;
    AddItemDirector *addItemDirector_;
    QMeshPlot *meshPlot_;
    QList<QMeshItem *> items_;
    ItemListModel *itemListModel_;
    MeshGenerator *meshGenerator_;
};

#endif // QMESH_H
