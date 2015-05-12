#include <QDebug>

#include "q3movedirector.h"
#include "q3selectdirector.h"
#include "q3boundaryeditor.h"
#include "q3boundary.h"
#include "ui_q3boundaryeditor.h"

/******************************************************************************
 * Алгоритм:
 * 1. Все текущие границы подсвечиваются (-)
 * 2. При клике по элементу скелета ищем границу, к которой он принадлежит
 * 2.1 Если границы нет
 * 2.1.1 Предлагаем выбрать тип из списка
 * 2.1.2 При выборе типа удаляем все элементы из лэйоута, создаем и загружаем
 *       виджет текущего типа границы с родителем - this
 * 2.1.3 При нажатии кнопки сохранить создаем новую границу, добавляем ее в
 *       список границ, указатель на тип границы сохраняем в границе,
 *       меняем родителя типа на саму границу
 * 2.2 Если граница найдена
 * 2.2.1 Удаляем все элементы из лэйоута, загружаем виджет типа границы
 * 2.2.2 При изменении типа граница в списке создаем новый виджет с родителем
 *       this и отображаем виджет в лэйоуте
 * 2.2.3 При нажатии кнопки сохранить, в случае, если тип поменялся - удаляем
 *       старый тип, сохраняем новый в границе, меняем родителя
 *****************************************************************************/

Q3BoundaryEditor::Q3BoundaryEditor(Q3Plot *plot, Q3Sceleton *sceleton,
                                   QList<Q3Boundary *> *boundaries,
                                   QWidget *parent) :
    Q3Director(Q3Director::Boundary, parent),
    ui(new Ui::Q3BoundaryEditor),
    boundaries_(boundaries),
    boundary_(NULL),
    boundaryType_(NULL),
    boundaryItem_(NULL)
{
    ui->setupUi(this);

    directorManager_ = new Q3DirectorManager(this);

    Q3Director *moveDirector = new Q3MoveDirector(this);

    directorManager_->addDirector(moveDirector);
    directorManager_->addDirector(this);

    foreach (Q3Director *director, directorManager_->directors())
        director->setSceleton(sceleton);

    directorManager_->setPlot(plot);
}

Q3BoundaryEditor::~Q3BoundaryEditor()
{
    if (boundaryItem_)
        boundaryItem_->setSelected(false);
    if (boundary_ && boundary_->type() == boundaryType_ && boundaryType_)
    {
        ui->boundaryLayout->removeWidget(boundaryType_);
        boundaryType_->setParent(NULL);
        boundaryType_->hide();
    }
    delete ui;
}

void Q3BoundaryEditor::draw(Q3Painter &painter) const
{

}

bool Q3BoundaryEditor::processClick(QMouseEvent *event, const QPointF &scenePos)
{
    if (boundary_)
    {
        if (boundary_->type() != boundaryType_)
            delete boundaryType_;
        else
        {
            ui->boundaryLayout->removeWidget(boundaryType_);
            boundaryType_->setParent(NULL);
            boundaryType_->hide();
        }
        boundaryType_ = NULL;
        boundary_ = NULL;
    }
    else
    {
        delete boundaryType_;
        boundaryType_ = NULL;
    }

    QLayoutItem *litem = NULL;
    while((litem = ui->boundaryLayout->takeAt(0)))
    {
        delete litem->widget();
        delete litem;
    }

    if (boundaryItem_)
    {
        boundaryItem_->setSelected(false);
        boundaryItem_ = NULL;
    }
    ui->boundaryTypeComboBox->clear();

    qreal radius = SelectRadius / plot_->sx();
    Q3SceletonItem *item = sceleton_->itemAt(scenePos, radius);

    if (item)
    {
        QList<Q3BoundaryType::Type> supportedBoundaries = \
                Q3BoundaryType::supportedBoundaryTypes(item->type());
        if (supportedBoundaries.empty())
            return false;

        boundaryItem_ = item;
        boundaryItem_->setSelected(true);

        ui->boundaryTypeComboBox->clear();
        ui->boundaryTypeComboBox->addItem("Не выбран",
                                          QVariant(static_cast<int>(-1)));
        foreach(Q3BoundaryType::Type type, supportedBoundaries)
        {
            ui->boundaryTypeComboBox->addItem(Q3BoundaryType::typeToString(type),
                                              QVariant(static_cast<int>(type)));
        }

        foreach(Q3Boundary *boundary, *boundaries_)
        {
            if (boundary->contains(item))
            {
                boundary_ = boundary;
                boundaryType_ = boundary->type();
                if (boundaryType_)
                {
                    int index = ui->boundaryTypeComboBox->findData( \
                                    QVariant(static_cast<int>(boundaryType_->toEnum())));
                    ui->boundaryTypeComboBox->setCurrentIndex(index);
                }
            }
        }

        return true;
    }

    return false;
}

void Q3BoundaryEditor::on_boundaryTypeComboBox_currentIndexChanged(int index)
{
    if (!boundary_ || boundaryType_ != boundary_->type())
    {
        delete boundaryType_;
    }
    else if (boundaryType_)
    {
        ui->boundaryLayout->removeWidget(boundaryType_);
        boundaryType_->setParent(NULL);
        boundaryType_->hide();
    }
    boundaryType_ = NULL;

    if (index == -1)
        return;

    int typeIndex = ui->boundaryTypeComboBox->itemData(index).toInt();

    if (typeIndex != -1)
    {
        Q3BoundaryType::Type type = static_cast<Q3BoundaryType::Type>(typeIndex);
        if (boundary_ && boundary_->type() && type == boundary_->type()->toEnum())
        {
            boundaryType_ = boundary_->type();
            ui->boundaryLayout->addWidget(boundaryType_);
            boundaryType_->show();
        }
        else
        {
            boundaryType_ = Q3BoundaryType::getBoundaryTypeByEnum(type);
            ui->boundaryLayout->addWidget(boundaryType_);
        }
    }
}

void Q3BoundaryEditor::on_saveBoundaryButton_clicked()
{
    if (!boundaryItem_ || !boundaryType_)
        return;

    if (!boundary_)
    {
        boundary_ = new Q3Boundary();
        boundary_->addItem(boundaryItem_);
        boundaries_->append(boundary_);
    }

    if (boundaryType_ != boundary_->type())
    {
        delete boundary_->type();
        boundary_->setType(boundaryType_);
    }

    boundary_->type()->save();
}

void Q3BoundaryEditor::on_removeBoundaryButton_clicked()
{
    if (!boundary_)
        return;
    ui->boundaryTypeComboBox->setCurrentIndex(0);
    boundaries_->removeAll(boundary_);
    delete boundary_;
}

void Q3BoundaryEditor::on_defaultBoundaryButton_clicked()
{
    foreach (Q3SceletonItem *item, sceleton_->items())
    {
        if (!Q3BoundaryType::supportedBoundaryTypes(item->type()).contains(Q3BoundaryType::NoSlipBoundary))
            continue;

        Q3Boundary *boundary = Q3Boundary::findByElement(boundaries_, item);
        if (!boundary)
        {
            boundary = new Q3Boundary();
            boundary->setTypeByEnum(Q3BoundaryType::NoSlipBoundary);
            boundary->addItem(item);
            boundaries_->append(boundary);
        }
    }
//    emit goToTab(2);
}
