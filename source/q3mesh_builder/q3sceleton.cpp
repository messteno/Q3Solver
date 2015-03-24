#include <QtAlgorithms>
#include <QDebug>
#include <QMessageBox>
#include <QtGlobal>
#include <QTime>

#include "q3sceleton.h"
#include "q3pointconnection.h"
#include "q3plot.h"
#include "q3itemvisitor.h"

const int Q3Sceleton::tableColumns_ = 3;

Q3Sceleton::Q3Sceleton(QWidget *parent) :
    QAbstractTableModel(parent),
    Q3PlotDrawable(),
    active_(true)
{
}

Q3Sceleton::~Q3Sceleton()
{
    
}

Q3SceletonItem* Q3Sceleton::itemAt(const QPointF &pos, qreal radius,
                                  Q3SceletonItem::Type type) const
{
    qreal minDistance = 0;
    Q3SceletonItem *nearestItem = NULL;

    foreach (Q3SceletonItem *item, items_)
    {
        if (type != Q3SceletonItem::Base && item->type() != type)
            continue;

        qreal distance = item->distanceTo(pos);
        if (distance > radius)
            continue;

        if (!nearestItem)
        {
            minDistance = distance;
            nearestItem = item;
        }
        else if (item->type() != Q3SceletonItem::PointConnection &&
                 distance < minDistance)
        {
            minDistance = distance;
            nearestItem = item;
        }
    }
    return nearestItem;
}

Q3SceletonItem *Q3Sceleton::itemToResizeAt(const QPointF &pos,
                                           qreal radius) const
{
    qreal minDistance = 0;
    Q3SceletonItem *nearestItem = NULL;

    foreach (Q3SceletonItem *item, items_)
    {
        if (!item->isResizable())
            continue;

        qreal distance = item->distanceFromBoundaryTo(pos);
        if (distance > radius)
            continue;

        if (!nearestItem)
        {
            minDistance = distance;
            nearestItem = item;
        }
        else if (item->type() != Q3SceletonItem::PointConnection &&
                 distance < minDistance)
        {
            minDistance = distance;
            nearestItem = item;
        }
    }
    return nearestItem;
}

void Q3Sceleton::addItem(Q3SceletonItem *item)
{
    beginInsertRows(QModelIndex(), items_.count() - 1, items_.count() - 1) ;
    items_.append(item);
    endInsertRows();

    QModelIndex top = index(items_.count() - 1, 0);
    QModelIndex bottom = index(items_.count() - 1, tableColumns_);

    emit dataChanged(top, bottom);
}

void Q3Sceleton::removeSelectedItems()
{
    // We need to select all connected items to delete them too
    foreach (Q3SceletonItem *item, items_)
    {
        if (!item->isSelected())
            continue;
        switch (item->type())
        {
            case Q3SceletonItem::Point:
            {
                Q3Point *point = dynamic_cast<Q3Point *>(item);
                if (point)
                {
                    foreach (Q3SceletonItem *testItem, items_)
                    {
                        Q3PointConnection *conn = \
                                dynamic_cast<Q3PointConnection *>(testItem);
                        if (conn && (conn->a() == item || conn->b() == item))
                            conn->setSelected(true);
                    }
                }
                break;
            }
            default:
                break;
        }
    }

    int itemCounter = 0;
    QMutableListIterator<Q3SceletonItem *> it(items_);
    while(it.hasNext())
    {
        Q3SceletonItem *item = it.next();
        if (item->isSelected())
        {
            beginRemoveRows(QModelIndex(), itemCounter, itemCounter);
            it.remove();
            delete item;
            endRemoveRows();
        }
        else
            itemCounter++;
    }
}

void Q3Sceleton::draw(Q3Painter &painter) const
{
    painter.save();
    QPen pen = painter.pen();
    foreach (Q3SceletonItem *item, items_)
    {
        painter.setBrush(item->backgroundColor());
        pen.setColor(item->penColor());
        painter.setPen(pen);
        item->draw(painter);
    }
    painter.restore();
}

bool Q3Sceleton::createMesh(Q3MeshAdapter *adapter)
{
    bool hasInput = false;
    bool hasOutput = false;

    outerBoundary_.clear();
    innerBoundaries_.clear();

    if (!items_.count())
    {
        QMessageBox::warning(NULL, tr("Q3Solver"),
                             tr("Невозможно создать сетку, "
                                "элементы отсутствуют"),
                             QMessageBox::Ok);
        return false;
    }

    emit createMeshProgress(10);

    // Проверить отсутствие пересечения элементов
    Q3ItemCrossVisitor crossVisitor;
    foreach (Q3SceletonItem *item1, items_)
    {
        foreach (Q3SceletonItem *item2, items_)
        {
            if (item1 == item2)
                continue;

            bool crosses = item1->accept(crossVisitor, item2);
            if (crosses)
            {
                QMessageBox::warning(NULL, tr("Q3Solver"),
                                     tr("Невозможно создать сетку, "
                                        "некоторые элементы пересекаются"),
                                     QMessageBox::Ok);
                return false;
            }
        }
    }

    emit createMeshProgress(20);

    // Найти самый левый элемент
    Q3ItemLeftmostVisitor leftmostVisitor;
    foreach (Q3SceletonItem *item, items_)
        item->accept(leftmostVisitor);
    Q3SceletonItem *leftmost = leftmostVisitor.leftmost();
    Q_ASSERT(leftmost);

    emit createMeshProgress(30);

    // Найти все связанные элементы
    // Проверить, что получилась замкнутая область
    Q3ItemConnectedVisitor connectedVisitor;
    leftmost->accept(connectedVisitor);
    while (!connectedVisitor.isFinished())
    {
        bool found = false;
        foreach (Q3SceletonItem *item, items_)
        {
            if (item->accept(connectedVisitor))
            {
                found = true;
                break;
            }
        }

        // Если не нашли нового соединения, попробуем сделать шаг назад
        if (!found)
            connectedVisitor.backward();
    }

    emit createMeshProgress(40);

    outerBoundary_ = connectedVisitor.connectedItems();

    if (outerBoundary_.empty())
    {
        QMessageBox::warning(NULL, tr("Q3Solver"),
                             tr("Невозможно создать сетку, "
                                "внешняя область не замкнута"),
                             QMessageBox::Ok);
        return false;
    }

    // TODO: проверить, что из каждой точки внешней границы выходит ровно два
    //       отрезка границы

    // Проверить, что все остальные элементы внутри выделенной области
    foreach (Q3SceletonItem *item, items_)
    {
        if (outerBoundary_.contains(item))
        {
            // Проверим на наличие втока и стока
            if (item->boundaryType() == Q3Mesh::InBoundary)
                hasInput = true;
            else if (item->boundaryType() == Q3Mesh::OutBoundary)
                hasOutput = true;

            continue;
        }

        // Проверим, что внутренние элементы не являются втоком или стоком
        if (item->boundaryType() != Q3Mesh::CannotBeBoundary &&
            item->boundaryType() != Q3Mesh::NotBoundary)
        {
            QMessageBox::warning(NULL, tr("Q3Solver"),
                                 tr("Невозможно создать сетку, "
                                    "внутренние элементы не могут быть"
                                    "втоком или стоком"),
                                 QMessageBox::Ok);
            return false;
        }

        Q3ItemRayTraceVisitor rayTraceVisitor(item);
        foreach (Q3SceletonItem *chainItem, outerBoundary_)
            chainItem->accept(rayTraceVisitor);

        if (rayTraceVisitor.rayCrossCount() % 2 == 0)
        {
            QMessageBox::warning(NULL, tr("Q3Solver"),
                                 tr("Невозможно создать сетку, "
                                    "внешняя область не односвязна"),
                                 QMessageBox::Ok);
            return false;
        }
    }

    // TODO: возможно нужно перенести работу с границами в отдельный модуль

    if (hasInput && !hasOutput)
    {
        QMessageBox::warning(NULL, tr("Q3Solver"),
                             tr("Невозможно создать сетку, "
                                "отсутствует сток"),
                             QMessageBox::Ok);
        return false;
    }

    if (hasOutput && !hasInput)
    {
        QMessageBox::warning(NULL, tr("Q3Solver"),
                             tr("Невозможно создать сетку, "
                                "отсутствует вток"),
                             QMessageBox::Ok);
        return false;
    }

    emit createMeshProgress(50);

    // TODO: возможно, переписать то, что выше =)

    // Сделать копию списка элементов
    // Убрать отрезки внешней границы
    QList<Q3SceletonItem *> activeItems = items_;
    foreach (Q3SceletonItem *item, outerBoundary_)
        activeItems.removeAll(item);

    // Сортируем элементы по оси OX
    QList<Q3SceletonItem *> leftOrderItems = items_;
    qSort(leftOrderItems.begin(), leftOrderItems.end(),
          Q3SceletonItem::lefterThan);

    // Ищем внутренние замкнутые границы
    foreach (Q3SceletonItem *item, leftOrderItems)
    {
        Q3ItemInnerBoundaryVisitor innerBoundaryVisitor(activeItems);
        bool found = item->accept(innerBoundaryVisitor);
        if (found)
        {
            QList<Q3SceletonItem *> boundary = \
                    innerBoundaryVisitor.getBoundary();
            foreach (Q3SceletonItem *item, boundary)
                activeItems.removeAll(item);
            innerBoundaries_.append(boundary);
        }
    }

    emit createMeshProgress(60);

    // Проверяем, что внутри внутренних областей нет других элементов
    QList<QList<Q3SceletonItem *> >::iterator it;
    for (it = innerBoundaries_.begin(); it != innerBoundaries_.end(); ++it)
    {
        foreach (Q3SceletonItem *item, items_)
        {
            if ((*it).contains(item) || outerBoundary_.contains(item))
                continue;

            Q3ItemRayTraceVisitor rayTraceVisitor(item);
            foreach (Q3SceletonItem *chainItem, *it)
                chainItem->accept(rayTraceVisitor);

            if (rayTraceVisitor.rayCrossCount() % 2 == 1)
            {
                QMessageBox::warning(NULL, tr("Q3Solver"),
                                     tr("Невозможно создать сетку, "
                                        "внутренняя обасть содержит элементы"),
                                     QMessageBox::Ok);
                return false;
            }
        }
    }

    emit createMeshProgress(70);

//    qsrand(3);
//    QColor color(qrand() % 0xff, qrand() % 0xff, qrand() %0xff);
//    foreach (Q3SceletonItem *item, outerBoundary_)
//    {
//        item->setBackgroundColor(color);
//        item->setPenColor(color);
//    }

//    for (it = innerBoundaries_.begin(); it != innerBoundaries_.end(); ++it)
//    {
//        QColor color(qrand() % 0xff, qrand() % 0xff, qrand() %0xff);
//        foreach (Q3SceletonItem *item, *it)
//        {
//            item->setBackgroundColor(color);
//            item->setPenColor(color);
//        }
//    }

    bool generated = adapter->generateMesh(items_,
                                           outerBoundary_,
                                           innerBoundaries_,
                                           activeItems);
    emit createMeshProgress(90);

    return generated;
}

QList<Q3SceletonItem *> Q3Sceleton::items() const
{
    return items_;
}

int Q3Sceleton::rowCount(const QModelIndex &parent) const
{
    return items_.count();
}

int Q3Sceleton::columnCount(const QModelIndex &parent) const
{
    return tableColumns_;
}

QVariant Q3Sceleton::data(const QModelIndex &index, int role) const
{
    if (role == Qt::DisplayRole)
    {
        QString cell;
        Q3SceletonItem *item = items_.at(index.row());
        switch (index.column())
        {
            case 0:
                cell = item->typeToString();
                break;
            case 1:
                cell = item->toString();
                break;
            case 2:
                return QVariant(static_cast<uint>(item->boundaryType()));
        }
        return QVariant(cell);
    }
    return QVariant();
}

bool Q3Sceleton::setData(const QModelIndex &index, const QVariant &value, int role)
{
    if (role == Qt::EditRole)
    {
        Q3SceletonItem *item = items_.at(index.row());
        if (index.column() == ColumnBoundary && item->canBeBoundary())
            item->setBoundaryType(static_cast<Q3Mesh::BoundaryType>(value.toUInt()));
    }
    return true;
}

QVariant Q3Sceleton::headerData(int section, Qt::Orientation orientation, int role) const
{
    if (role == Qt::DisplayRole)
    {
        QString header;
        if (orientation == Qt::Horizontal)
        {
            switch (section)
            {
                case ColumnElement:
                    header = "Элемент";
                    break;
                case ColumnParameters:
                    header = "Параметры";
                    break;
                case ColumnBoundary:
                    header = "Граница";
                    break;
            }
        }
        else
            header = QString::number(section + 1);
        return QVariant(header);
    }
    return QVariant();
}

Qt::ItemFlags Q3Sceleton::flags(const QModelIndex &index) const
{
    Q3SceletonItem *item = items_.at(index.row());
    Qt::ItemFlags flags = QAbstractItemModel::flags(index);
    if (index.column() == ColumnBoundary && active_ && item->canBeBoundary())
        flags |= Qt::ItemIsEditable;
    return flags;
}

void Q3Sceleton::itemsUpdated()
{
    QModelIndex top = index(0, 0);
    QModelIndex bottom = index(items_.count() - 1, tableColumns_);

    emit dataChanged(top, bottom);
}

bool Q3Sceleton::isActive() const
{
    return active_;
}

void Q3Sceleton::setActive(bool active)
{
    active_ = active;
}
