/********************************************************************************
** Form generated from reading UI file 'pointadditemwidget.ui'
**
** Created by: Qt User Interface Compiler version 5.2.1
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_POINTADDITEMWIDGET_H
#define UI_POINTADDITEMWIDGET_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QFormLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_PointAddItemWidget
{
public:
    QFormLayout *formLayout;
    QPushButton *pushButton;
    QLabel *xLabel;
    QLineEdit *xEdit;
    QLabel *yLabel;
    QLineEdit *yEdit;
    QCheckBox *snapToGrid;

    void setupUi(QWidget *PointAddItemWidget)
    {
        if (PointAddItemWidget->objectName().isEmpty())
            PointAddItemWidget->setObjectName(QStringLiteral("PointAddItemWidget"));
        PointAddItemWidget->resize(306, 122);
        formLayout = new QFormLayout(PointAddItemWidget);
        formLayout->setObjectName(QStringLiteral("formLayout"));
        formLayout->setFieldGrowthPolicy(QFormLayout::AllNonFixedFieldsGrow);
        pushButton = new QPushButton(PointAddItemWidget);
        pushButton->setObjectName(QStringLiteral("pushButton"));

        formLayout->setWidget(3, QFormLayout::SpanningRole, pushButton);

        xLabel = new QLabel(PointAddItemWidget);
        xLabel->setObjectName(QStringLiteral("xLabel"));

        formLayout->setWidget(0, QFormLayout::LabelRole, xLabel);

        xEdit = new QLineEdit(PointAddItemWidget);
        xEdit->setObjectName(QStringLiteral("xEdit"));

        formLayout->setWidget(0, QFormLayout::FieldRole, xEdit);

        yLabel = new QLabel(PointAddItemWidget);
        yLabel->setObjectName(QStringLiteral("yLabel"));

        formLayout->setWidget(1, QFormLayout::LabelRole, yLabel);

        yEdit = new QLineEdit(PointAddItemWidget);
        yEdit->setObjectName(QStringLiteral("yEdit"));

        formLayout->setWidget(1, QFormLayout::FieldRole, yEdit);

        snapToGrid = new QCheckBox(PointAddItemWidget);
        snapToGrid->setObjectName(QStringLiteral("snapToGrid"));
        snapToGrid->setChecked(true);

        formLayout->setWidget(2, QFormLayout::SpanningRole, snapToGrid);


        retranslateUi(PointAddItemWidget);

        QMetaObject::connectSlotsByName(PointAddItemWidget);
    } // setupUi

    void retranslateUi(QWidget *PointAddItemWidget)
    {
        PointAddItemWidget->setWindowTitle(QApplication::translate("PointAddItemWidget", "Form", 0));
        pushButton->setText(QApplication::translate("PointAddItemWidget", "\320\224\320\276\320\261\320\260\320\262\320\270\321\202\321\214 \321\202\320\276\321\207\320\272\321\203", 0));
        xLabel->setText(QApplication::translate("PointAddItemWidget", "X:", 0));
        yLabel->setText(QApplication::translate("PointAddItemWidget", "Y:", 0));
        snapToGrid->setText(QApplication::translate("PointAddItemWidget", "\320\277\321\200\320\270\320\262\321\217\320\267\320\272\320\260 \320\272 \321\201\320\265\321\202\320\272\320\265", 0));
    } // retranslateUi

};

namespace Ui {
    class PointAddItemWidget: public Ui_PointAddItemWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_POINTADDITEMWIDGET_H
