#include "q3painter.h"

Q3Painter::Q3Painter()
{

}

Q3Painter::~Q3Painter()
{

}

void Q3Painter::doScale(qreal scaleX, qreal scaleY)
{
    scaleX_ = scaleX;
    scaleY_ = scaleY;
}

qreal Q3Painter::sx()
{
    return scaleX_;
}

qreal Q3Painter::sy()
{
    return scaleY_;
}
