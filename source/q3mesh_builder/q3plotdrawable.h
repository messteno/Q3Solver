#ifndef Q3PLOTDRAWABLE_H
#define Q3PLOTDRAWABLE_H

#include <QWidget>

#include "q3painter.h"

class Q3PlotDrawable : public QWidget
{
    Q_OBJECT

public:
    Q3PlotDrawable(QWidget *parent = NULL);
    virtual ~Q3PlotDrawable();

    virtual void draw(Q3Painter &painter) const = 0;
};

#endif // Q3PLOTDRAWABLE_H
