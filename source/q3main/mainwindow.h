#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include "q3sceletoneditor.h"
#include "q3solver.h"

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

private slots:
    void on_actionMeshBuilder_triggered();

private:
    Ui::MainWindow *ui;
    Q3Solver *solver_;
};

#endif // MAINWINDOW_H
