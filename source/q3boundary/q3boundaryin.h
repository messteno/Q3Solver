#ifndef Q3BOUNDARYIN_H
#define Q3BOUNDARYIN_H

#include "q3boundarytype.h"
#include "q3xyexpression.h"
#include <QMessageBox>

namespace Ui {
class Q3BoundaryIn;
}

class Q3BoundaryIn : public Q3BoundaryType
{
    Q_OBJECT

public:
    explicit Q3BoundaryIn(QWidget *parent = 0);
    ~Q3BoundaryIn();

    QVector2D velocity(Q3SceletonItem *item,
                       const QPointF &a, const QPointF &b, qreal time);
    void setVelocityText(const QString &vXText, const QString &vYText);
    virtual void save();

private:
    Ui::Q3BoundaryIn *ui;
    Q3XYExpression velocityXExp_;
    Q3XYExpression velocityYExp_;
};

#endif // Q3BOUNDARYIN_H
