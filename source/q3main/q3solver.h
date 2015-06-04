#ifndef Q3SOLVER_H
#define Q3SOLVER_H

#include <QWidget>
#include <QList>

#include "q3directormanager.h"
#include "q3mesh.h"
#include "q3calc.h"
#include "q3sceleton.h"
#include "q3boundary.h"
#include "q3sceletoneditor.h"
#include "q3mesheditor.h"
#include "q3boundaryeditor.h"
#include "q3calculuseditor.h"

namespace Ui {
class Q3Solver;
}

class Q3Solver : public QWidget
{
    Q_OBJECT

public:
    explicit Q3Solver(QWidget *parent = 0);
    ~Q3Solver();

    void paintEvent(QPaintEvent *event);

    Q3Mesh& mesh();
    Q3Sceleton& sceleton();

private slots:
    void on_tabWidget_currentChanged(int index);

private:
    Ui::Q3Solver *ui;
    Q3Mesh mesh_;
    Q3Sceleton sceleton_;
    QList<Q3Boundary *> boundaries_;

    Q3SceletonEditor *sceletonEditor_;
    Q3MeshEditor *meshEditor_;
    Q3BoundaryEditor *boundaryEditor_;
    Q3CalculusEditor *calculusEditor_;

    int tabWidgetIndex_;
};

#endif // Q3SOLVER_H
