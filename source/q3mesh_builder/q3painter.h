#ifndef Q3PAINTER_H
#define Q3PAINTER_H

#include <QPainter>

class Q3Painter : public QPainter
{
private:
    qreal scaleX_;
    qreal scaleY_;
public:
    Q3Painter();
    virtual ~Q3Painter();
    virtual void scale(qreal scaleX_, qreal scaleY_);

    qreal sx();
    qreal sy();
};

#endif // Q3PAINTER_H
