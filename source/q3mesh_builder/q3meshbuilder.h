#ifndef Q3MESHBUILDER_H
#define Q3MESHBUILDER_H

#include <QWidget>

#include "q3sceleton.h"
#include "q3plot.h"

namespace Ui {
class Q3MeshBuilder;
}

class Q3MeshBuilder : public QWidget
{
    Q_OBJECT

public:
    static const int SelectRadius;

    enum ProcessMode
    {
        NoProcess,
        MoveProcess,
        MoveItemProcess,
        AddItemProcess,
    };

    explicit Q3MeshBuilder(QWidget *parent = 0);
    ~Q3MeshBuilder();

    ProcessMode processMode() const;
    void setProcessMode(ProcessMode mode);

    Q3SceletonItem::Type itemType() const;
    void setItemType(Q3SceletonItem::Type type);

private slots:

    // in local coordinates
    void plotMouseClicked(const QPointF scenePos);
    void plotMouseDragged(const QPointF oldScenePos, const QPointF newScenePos);
    void plotMouseDropped(const QPointF scenePos);

    void on_pointButton_clicked(bool checked);
    void on_pointConnectionButton_clicked(bool checked);

private:
    Q3Sceleton *sceleton_;
    Q3SceletonItem *addItem_;
    ProcessMode processMode_;

    Q3SceletonItem::Type itemType_;

    QList<Q3SceletonItem*> selectedItems_;

    Ui::Q3MeshBuilder *ui;
};

#endif // Q3MESHBUILDER_H
