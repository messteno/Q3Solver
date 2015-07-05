#include <QDebug>

#include "q3directormanager.h"

Q3DirectorManager::Q3DirectorManager(QWidget *parent) :
    QWidget(parent)
{

}

Q3DirectorManager::~Q3DirectorManager()
{
}

void Q3DirectorManager::plotKeyReleaseEvent(QKeyEvent *event)
{
    QList<Q3Director *> orderedByActivityDirectors =
            Q3Director::orderListByActivity(directors_);
    foreach(Q3Director *director, orderedByActivityDirectors)
    {
        if (!director->isEnabled())
            continue;

        bool processed = director->processKeyRelease(event->key());
        if (processed)
        {
            emit plotUpdate();
            break;
        }
    }
}

void Q3DirectorManager::addDirector(Q3Director *director)
{
    directors_.append(director);
}

QList<Q3Director *> Q3DirectorManager::directors() const
{
    return directors_;
}

void Q3DirectorManager::setPlot(Q3Plot *plot)
{
    foreach (Q3Director *director, directors_)
        director->setPlot(plot);

    connect(plot, SIGNAL(mouseClicked(QMouseEvent *, const QPointF)),
            this, SLOT(plotMouseClicked(QMouseEvent *, const QPointF)));
    connect(plot, SIGNAL(mouseDragged(const QPointF, const QPointF)),
            this, SLOT(plotMouseDragged(const QPointF, const QPointF)));
    connect(plot, SIGNAL(mouseDropped(const QPointF)),
            this, SLOT(plotMouseDropped(const QPointF)));
    connect(plot, SIGNAL(mouseMoved(const QPointF, const QPointF)),
            this, SLOT(plotMouseMoved(const QPointF, const QPointF)));
    connect(plot, SIGNAL(keyReleased(QKeyEvent *)),
            this, SLOT(plotKeyReleaseEvent(QKeyEvent *)));

    connect(this, SIGNAL(plotUpdate()), plot, SLOT(update()));
}

void Q3DirectorManager::plotMouseClicked(QMouseEvent *event,
                                         const QPointF &scenePos)
{
    QList<Q3Director *> orderedByActivityDirectors =
            Q3Director::orderListByActivity(directors_);
    foreach(Q3Director *director, orderedByActivityDirectors)
    {
        if (!director->isEnabled())
            continue;

        bool processed = director->processClick(event, scenePos);
        if (processed)
            break;
    }
    emit plotUpdate();
}

void Q3DirectorManager::plotMouseDragged(const QPointF &oldScenePos,
                                         const QPointF &newScenePos)
{
    QList<Q3Director *> orderedByActivityDirectors =
            Q3Director::orderListByActivity(directors_);
    foreach(Q3Director *director, orderedByActivityDirectors)
    {
        if (!director->isEnabled())
            continue;

        bool processed = director->processDragged(oldScenePos,
                                                  newScenePos);
        if (processed)
            break;
    }
    emit plotUpdate();
}

void Q3DirectorManager::plotMouseDropped(const QPointF &scenePos)
{
    QList<Q3Director *> orderedByActivityDirectors =
            Q3Director::orderListByActivity(directors_);
    foreach(Q3Director *director, orderedByActivityDirectors)
    {
        if (!director->isEnabled())
            continue;

        bool processed = director->processDropped(scenePos);
        if (processed)
            break;
    }
    emit plotUpdate();
}

void Q3DirectorManager::plotMouseMoved(const QPointF &oldScenePos,
                                       const QPointF &newScenePos)
{
    QList<Q3Director *> orderedByActivityDirectors =
            Q3Director::orderListByActivity(directors_);
    foreach(Q3Director *director, orderedByActivityDirectors)
    {
        if (!director->isEnabled())
            continue;

        bool processed = director->processMoved(oldScenePos,
                                                newScenePos);
        if (processed)
            break;
    }
    emit plotUpdate();
}

