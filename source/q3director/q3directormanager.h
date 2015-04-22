#ifndef Q3DIRECTORMANAGER_H
#define Q3DIRECTORMANAGER_H

#include <QList>
#include <QObject>

#include "q3plot.h"
#include "q3director.h"

class Q3DirectorManager : public QObject
{
    Q_OBJECT
public:
    Q3DirectorManager(QObject *parent = NULL);
    ~Q3DirectorManager();

    void addDirector(Q3Director *director);
    QList<Q3Director *> directors() const;

    void setPlot(Q3Plot *plot);

public slots:
    // in local coordinates
    void plotMouseClicked(QMouseEvent *event, const QPointF &scenePos);
    void plotMouseDragged(const QPointF &oldScenePos, const QPointF &newScenePos);
    void plotMouseDropped(const QPointF &scenePos);
    void plotMouseMoved(const QPointF &oldScenePos, const QPointF &newScenePos);
    void plotKeyReleaseEvent(QKeyEvent *event);

signals:
    void plotUpdate();

private:
    QList<Q3Director *> directors_;
};

#endif // Q3DIRECTORMANAGER_H
