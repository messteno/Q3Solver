#ifndef QMESHITEMWIDGET_H
#define QMESHITEMWIDGET_H

#include <QPushButton>
#include <QWidget>

class QMeshItemWidget : public QWidget
{
    Q_OBJECT

public:
    explicit QMeshItemWidget(QWidget *parent = 0);
    virtual ~QMeshItemWidget();

    QPushButton *addButton_;

public slots:
    //virtual void addItemAction() = 0;
    void showWidget();

protected:
    QString widgetName_;
};

#endif // QMESHITEMWIDGET_H
