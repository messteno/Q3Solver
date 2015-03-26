#ifndef Q3SOLVER_H
#define Q3SOLVER_H

#include <QWidget>

#include "q3directormanager.h"
#include "q3mesh.h"
#include "q3sceleton.h"

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

    Q3Mesh *mesh() const;
    Q3Sceleton *sceleton() const;

private:
    Ui::Q3Solver *ui;
    Q3DirectorManager *directorManager_;
    Q3Mesh *mesh_;
    Q3Sceleton *sceleton_;
};

#endif // Q3SOLVER_H
