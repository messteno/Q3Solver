#ifndef Q3CALC_H
#define Q3CALC_H

#include <QThread>
#include <QFile>
#include <QTime>
#include <QTextStream>

#include "q3mesh.h"

class Q3Calc : public QThread
{
    Q_OBJECT

public:
    Q3Calc(Q3Mesh &mesh, qreal tau, qreal Re, QObject *parent = 0);
    ~Q3Calc();

    void abort();
    void reset();
    QString info();

    void setMonotoneTerm(bool monotoneTerm);
    void setBadTriangleFix(bool badTriangleFix);
    void setTau(const qreal &tau);
    void setRe(const qreal &Re);

protected:
    void run() Q_DECL_OVERRIDE;

signals:
    void calcStepEnded(qreal time);

private:
    void prepare();
    void predictor();
    void corrector();
    void calcFaithfulResidualNS();
    void calcFaithfulResidualDiv();

    // TODO: переместить в отдельный класс
    static void incompleteCholesky(qreal *AN, int *JA, int *IA, int n);

    Q3Mesh &mesh_;
    bool abort_;
    bool started_;

    static const int maxPredictorIterationsCount;
    static const qreal maxPredictorError;

    QVector<qreal> AN_;
    QVector<qreal> MN_;
    QVector<int> JA_;
    QVector<int> IA_;
    QVector<qreal> BN_;
    QVector<qreal> XN_;
    QVector<qreal> TN_;

    bool badTriangleFix_;
    bool monotoneTerm_;

    qreal tau_;
    qreal Re_;

    qreal residual_;
    qreal faithfulResidualNS_;
    qreal faithfulResidualDiv_;

    qreal time_;

    quint64 calcTime_;
    QTime calcTimer_;
};

#endif // Q3CALC_H
