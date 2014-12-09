#include "q3meshbuilder.h"
#include "ui_q3meshbuilder.h"

Q3MeshBuilder::Q3MeshBuilder(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::Q3MeshBuilder)
{
    ui->setupUi(this);
}

Q3MeshBuilder::~Q3MeshBuilder()
{
    delete ui;
}
