/********************************************************************************
** Form generated from reading UI file 'lineadditemwidget.ui'
**
** Created by: Qt User Interface Compiler version 5.2.1
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_LINEADDITEMWIDGET_H
#define UI_LINEADDITEMWIDGET_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QFormLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_LineAddItemWidget
{
public:
    QFormLayout *formLayout;
    QLabel *x1Label;
    QLineEdit *x1Edit;
    QLabel *y1Label;
    QLineEdit *y1Edit;
    QLabel *x2Label;
    QLineEdit *x2Edit;
    QLabel *y2Label;
    QLineEdit *y2Edit;
    QPushButton *pushButton;

    void setupUi(QWidget *LineAddItemWidget)
    {
        if (LineAddItemWidget->objectName().isEmpty())
            LineAddItemWidget->setObjectName(QStringLiteral("LineAddItemWidget"));
        LineAddItemWidget->resize(311, 164);
        formLayout = new QFormLayout(LineAddItemWidget);
        formLayout->setObjectName(QStringLiteral("formLayout"));
        formLayout->setFieldGrowthPolicy(QFormLayout::AllNonFixedFieldsGrow);
        x1Label = new QLabel(LineAddItemWidget);
        x1Label->setObjectName(QStringLiteral("x1Label"));

        formLayout->setWidget(1, QFormLayout::LabelRole, x1Label);

        x1Edit = new QLineEdit(LineAddItemWidget);
        x1Edit->setObjectName(QStringLiteral("x1Edit"));
        x1Edit->setEnabled(true);
        x1Edit->setReadOnly(true);

        formLayout->setWidget(1, QFormLayout::FieldRole, x1Edit);

        y1Label = new QLabel(LineAddItemWidget);
        y1Label->setObjectName(QStringLiteral("y1Label"));

        formLayout->setWidget(3, QFormLayout::LabelRole, y1Label);

        y1Edit = new QLineEdit(LineAddItemWidget);
        y1Edit->setObjectName(QStringLiteral("y1Edit"));
        y1Edit->setEnabled(true);
        y1Edit->setReadOnly(true);

        formLayout->setWidget(3, QFormLayout::FieldRole, y1Edit);

        x2Label = new QLabel(LineAddItemWidget);
        x2Label->setObjectName(QStringLiteral("x2Label"));

        formLayout->setWidget(4, QFormLayout::LabelRole, x2Label);

        x2Edit = new QLineEdit(LineAddItemWidget);
        x2Edit->setObjectName(QStringLiteral("x2Edit"));
        x2Edit->setEnabled(true);
        x2Edit->setReadOnly(true);

        formLayout->setWidget(4, QFormLayout::FieldRole, x2Edit);

        y2Label = new QLabel(LineAddItemWidget);
        y2Label->setObjectName(QStringLiteral("y2Label"));

        formLayout->setWidget(5, QFormLayout::LabelRole, y2Label);

        y2Edit = new QLineEdit(LineAddItemWidget);
        y2Edit->setObjectName(QStringLiteral("y2Edit"));
        y2Edit->setEnabled(true);
        y2Edit->setReadOnly(true);

        formLayout->setWidget(5, QFormLayout::FieldRole, y2Edit);

        pushButton = new QPushButton(LineAddItemWidget);
        pushButton->setObjectName(QStringLiteral("pushButton"));

        formLayout->setWidget(6, QFormLayout::SpanningRole, pushButton);


        retranslateUi(LineAddItemWidget);

        QMetaObject::connectSlotsByName(LineAddItemWidget);
    } // setupUi

    void retranslateUi(QWidget *LineAddItemWidget)
    {
        LineAddItemWidget->setWindowTitle(QApplication::translate("LineAddItemWidget", "Form", 0));
        x1Label->setText(QApplication::translate("LineAddItemWidget", "X1", 0));
        y1Label->setText(QApplication::translate("LineAddItemWidget", "Y1", 0));
        x2Label->setText(QApplication::translate("LineAddItemWidget", "X2", 0));
        y2Label->setText(QApplication::translate("LineAddItemWidget", "Y2", 0));
        pushButton->setText(QApplication::translate("LineAddItemWidget", "\320\224\320\276\320\261\320\260\320\262\320\270\321\202\321\214 \320\276\321\202\321\200\320\265\320\267\320\276\320\272", 0));
    } // retranslateUi

};

namespace Ui {
    class LineAddItemWidget: public Ui_LineAddItemWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_LINEADDITEMWIDGET_H
