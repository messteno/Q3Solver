#ifndef Q3SCELETONITEM_H
#define Q3SCELETONITEM_H

#include <q3painter.h>

class Q3SceletonItem
{
public:
    enum Type
    {
        Base,
        PointConnection,
        Point,
    };

    static const QColor DefaultColor;
    static const QColor SelectedColor;

    Q3SceletonItem(Type type);
    virtual ~Q3SceletonItem();

    virtual void draw(Q3Painter &painter) = 0;
    virtual qreal distanceTo(const QPointF &pos) const = 0;
    virtual QRectF boundingRect() const = 0;
    virtual void move(const QPointF diff) = 0;

    bool isSelectable() const;
    void setSelectable(bool selectable);

    bool isSelected() const ;
    void setSelected(bool selected);

    Type type();

    void setColor(const QColor &color);
    void setSelectedColor(const QColor &color);

    const QColor &color() const;

protected:
    Type type_;

private:
    bool selected_;
    bool selectable_;
    QColor color_;
    QColor selectedColor_;
};

#endif // Q3SCELETONITEM_H
