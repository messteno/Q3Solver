#ifndef QMESH_H
#define QMESH_H

#include <QWidget>
#include "qmeshitemwidget.h"

namespace Ui {
class QMesh;
}

class QMesh : public QWidget
{
    Q_OBJECT

public:
    explicit QMesh(QWidget *parent = 0);
    ~QMesh();

    enum qmeshState
    {
        stateNone,
        stateElementSelection,
        stateElementAdding,
    };

    void setState(QMesh::qmeshState state);

private slots:
    void on_addElementButton_clicked();
    void on_cancelElementButton_clicked();
    void on_widgetElementButton_clicked();

private:
    Ui::QMesh *ui;
    qmeshState state_;
    QList<QMeshItemWidget *> itemWidgetlist_;

    void addElementButtons();
    void clearWidgetsLayout();
};

#endif // QMESH_H
