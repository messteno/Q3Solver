#include "additemdirector.h"
#include "ui_lineadditemwidget.h"
#include "lineadditemwidget.h"

LineAddItemWidget::LineAddItemWidget(AddItemDirector *director, QWidget *parent) :
    AddItemWidget(director, parent),
    ui(new Ui::LineAddItemWidget)
{
    ui->setupUi(this);
    connect(ui->pushButton, SIGNAL(clicked()), this, SLOT(selected()));
    expanded_ = true;
}

void LineAddItemWidget::expand()
{
    expanded_ = true;
}

void LineAddItemWidget::shrink()
{
    expanded_ = false;
}

QMeshItem* LineAddItemWidget::getItem()
{
    if (expanded_ == false)
        return NULL;
    return NULL;
}
