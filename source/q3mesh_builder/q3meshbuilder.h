#ifndef Q3MESHBUILDER_H
#define Q3MESHBUILDER_H

#include <QWidget>

#include "q3plot.h"

namespace Ui {
class Q3MeshBuilder;
}

class Q3MeshBuilder : public QWidget
{
    Q_OBJECT

public:
    explicit Q3MeshBuilder(QWidget *parent = 0);
    ~Q3MeshBuilder();

private:
    Ui::Q3MeshBuilder *ui;
};

#endif // Q3MESHBUILDER_H
