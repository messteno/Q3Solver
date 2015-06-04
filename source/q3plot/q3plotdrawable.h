#ifndef Q3PLOTDRAWABLE_H
#define Q3PLOTDRAWABLE_H

#include <QWidget>

#include "q3painter.h"

// Напоминание: не стоит пытаться наследовать от QObject
// У QObject приватный конструктор копирования,
// из-за этого слишком много ненужных проблем
class Q3PlotDrawable
{
public:
    Q3PlotDrawable();
    virtual ~Q3PlotDrawable();

    virtual void draw(Q3Painter &painter) const = 0;
};

#endif // Q3PLOTDRAWABLE_H
