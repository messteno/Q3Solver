#ifndef QMESH_H
#define QMESH_H

#include <QWidget>
#include "additemdirector.h"
#include "qmeshitem.h"

namespace Ui {
class QMesh;
}

class QMesh : public QWidget
{
    Q_OBJECT

public:
    explicit QMesh(QWidget *parent = 0);
    ~QMesh();

private slots:

    void on_addElementButton_clicked();
    void on_cancelElementButton_clicked();

private:
    Ui::QMesh *ui;
    AddItemDirector *addItemDirector_;
};

#endif // QMESH_H
