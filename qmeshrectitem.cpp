#include "qmeshrectitem.h"

QMeshRectItem::QMeshRectItem() :
    QMeshItem()
{
}

QMeshRectItem::QMeshRectItem(QRectF rect)
{
    rect_ = rect;
}

void QMeshRectItem::draw(QPainter &painter, qreal scaleX, qreal scaleY)
{
    QPen pen(QColor(0x00, 0x00, 0x00));
    painter.setPen(pen);
    QRectF paintRect = QRectF(rect_.x() * scaleX, rect_.y() * scaleY,
                              rect_.width() * scaleX, rect_.height() * scaleY);
    painter.fillRect(paintRect, painter.brush());
}
