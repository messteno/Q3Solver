#ifndef Q3CALC_H
#define Q3CALC_H

#include <QThread>
#include <QFile>
#include <QTextStream>

#include "q3mesh.h"

class Q3Calc : public QThread
{
    Q_OBJECT

public:
    Q3Calc(Q3Mesh *mesh, qreal tau, qreal Re, QObject *parent = 0);
    ~Q3Calc();

    void abort();
    void reset();
    QString info();

    void setBadTriangleFix(bool badTriangleFix);
    void setTau(const qreal &tau);
    void setRe(const qreal &Re);

protected:
    void run() Q_DECL_OVERRIDE;

signals:
    void updateInfo();

private:
    void prepare();
    void predictor();
    void corrector();

    // TODO: переместить в отдельный класс
    static void incompleteCholesky(qreal *AN, int *JA, int *IA, int n);

    Q3Mesh *mesh_;
    bool abort_;
    bool started_;

    const static int maxPredictorIterationsCount;
    const static qreal maxPredictorError;

    QVector<qreal> AN_;
    QVector<qreal> MN_;
    QVector<int> JA_;
    QVector<int> IA_;
    QVector<qreal> BN_;
    QVector<qreal> XN_;
    QVector<qreal> TN_;

    bool badTriangleFix_;

    qreal tau_;
    qreal Re_;

    qreal residual_;

    qreal time_;
};

template<class T>
void vectorToFile(QVector<T> &vec, const char *fileName)
{
    QFile file(fileName);
    file.open(QIODevice::WriteOnly | QIODevice::Text);
    QTextStream out(&file);
    out.setRealNumberPrecision(7);
    out.setRealNumberNotation(QTextStream::FixedNotation);
    foreach (const T& elem, vec)
        out << elem << "\n";
    file.close();
}

#endif // Q3CALC_H

