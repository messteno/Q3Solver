#ifndef Q3BOUNDARYEDITOR_H
#define Q3BOUNDARYEDITOR_H

#include <QWidget>
#include <QList>

#include "q3plot.h"
#include "q3sceleton.h"
#include "q3boundary.h"
#include "q3directormanager.h"

namespace Ui {
class Q3BoundaryEditor;
}

// Грзяный хак, но почему бы не наследовать Q3Director
class Q3BoundaryEditor : public Q3Director
{
    Q_OBJECT

public:
    explicit Q3BoundaryEditor(Q3Plot *plot, Q3Sceleton *sceleton,
                              QList<Q3Boundary *> *boundaries,
                              QWidget *parent = 0);
    ~Q3BoundaryEditor();

    virtual void draw(Q3Painter &painter) const;
    virtual bool processClick(QMouseEvent *event, const QPointF &scenePos);

private slots:
    void on_boundaryTypeComboBox_currentIndexChanged(int index);

    void on_saveBoundaryButton_clicked();

    void on_removeBoundaryButton_clicked();

    void on_defaultBoundaryButton_clicked();

signals:
    void goToTab(int tab);

private:
    Ui::Q3BoundaryEditor *ui;
    Q3DirectorManager *directorManager_;
    Q3SceletonItem *boundaryItem_;
    Q3BoundaryType *boundaryType_;
    Q3Boundary *boundary_;
    QList<Q3Boundary *> *boundaries_;
};

#endif // Q3BOUNDARYEDITOR_H
