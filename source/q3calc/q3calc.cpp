#include <QDebug>
#include <QTime>

#include <qmath.h>

#include "q3calc.h"
#include "conjugategradient.h"
#include "bicgstablinearsolver.h"
#include "preconditioner.h"

const int Q3Calc::maxPredictorIterationsCount = 1000;
const qreal Q3Calc::maxPredictorError = 1e-6;

// Возможно переделать геттеры в треугольниках на ссылки

Q3Calc::Q3Calc(Q3Mesh &mesh, qreal tau, qreal Re, QObject *parent) :
    QThread(parent),
    mesh_(mesh),
    tau_(tau),
    Re_(Re),
    residual_(0),
    time_(0),
    started_(false),
    abort_(false),
    badTriangleFix_(false),
    monotoneTerm_(true),
    faithfulResidualDiv_(0),
    faithfulResidualNS_(0),
    calcTime_(0)
{
}

Q3Calc::~Q3Calc()
{
    abort_ = true;
    wait();
}

void Q3Calc::run()
{
    calcTimer_.start();

    started_ = true;
    abort_ = false;
    prepare();
    while(!abort_)
    {
        // QTime timer;

        // timer.start();
        predictor();
        // qDebug() << "Predictor time:" << timer.elapsed();

        // timer.start();
        corrector();
        // qDebug() << "Corrector time: " << timer.elapsed();

//        calcFaithfulResidualNS();
        calcFaithfulResidualDiv();

        time_ += tau_;
        emit calcStepEnded(time_);

//        QFile pr("/home/mesteno/pr.txt");
//        if (pr.open(QFile::WriteOnly | QFile::Truncate))
//        {
//            QTextStream out(&pr);
//            for (int trIndex = 0; trIndex < mesh_.triangles().count(); ++trIndex)
//            {
//                Q3MeshTriangle *triangle = mesh_.triangles().at(trIndex);
//                out << trIndex << " "
//                    << QString::number(triangle->predictorVelocity().x(), 'd', 6) << " "
//                    << QString::number(triangle->predictorVelocity().y(), 'd', 6) << " "
//                    << "\n";
//            }
//            pr.close();
//        }

//        QFile co("/home/mesteno/co.txt");
//        if (co.open(QFile::WriteOnly | QFile::Truncate))
//        {
//            QTextStream out(&co);
//            for (int trIndex = 0; trIndex < mesh_.triangles().count(); ++trIndex)
//            {
//                Q3MeshTriangle *triangle = mesh_.triangles().at(trIndex);
//                out << trIndex << " "
//                    << QString::number(triangle->correctorVelocity().x(), 'd', 6) << " "
//                    << QString::number(triangle->correctorVelocity().y(), 'd', 6) << " "
//                    << "\n";
//            }
//            co.close();
//        }

        break;
    }
    calcTime_ += calcTimer_.elapsed();
}

void Q3Calc::prepare()
{
    // TODO:
    int trianglesCount = mesh_.triangles().count();

    AN_.clear();
    JA_.clear();
    IA_.clear();
    BN_.clear();
    XN_.clear();
    TN_.clear();

    AN_.fill(0, 3 * trianglesCount);
    JA_.fill(0, 3 * trianglesCount);
    IA_.fill(0, trianglesCount + 1);
    BN_.fill(0, trianglesCount);
    XN_.fill(0, trianglesCount);
    TN_.fill(0, 4 * trianglesCount);

    int anIndex = 0;
    for (int trInd = 0; trInd < trianglesCount; ++trInd)
    {
        Q3MeshTriangle *triangle = mesh_.triangles().at(trInd);

        for (int adjTrInd = 0; adjTrInd < triangle->adjacentTriangles().count();
             ++adjTrInd)
        {
            qreal S = triangle->edges().at(adjTrInd)->length();
            qreal dL = triangle->distanceToTriangles().at(adjTrInd);
            if (triangle->adjacentTriangles().at(adjTrInd))
                AN_[anIndex] += tau_ * S / dL;
        }

        AN_[anIndex] /= triangle->square();
        JA_[anIndex] = trInd;
        IA_[trInd] = anIndex;
        anIndex++;

        for (int adjTrInd = 0; adjTrInd < triangle->adjacentTriangles().count();
             ++adjTrInd)
        {
            Q3MeshTriangle *adjTr = triangle->adjacentTriangles().at(adjTrInd);
            if (!adjTr)
                continue;
            if (adjTr->id() < triangle->id())
                continue;
            qreal S = triangle->edges().at(adjTrInd)->length();
            qreal dL = triangle->distanceToTriangles().at(adjTrInd);
            AN_[anIndex] = -tau_ * S / dL;
            AN_[anIndex] /= triangle->square();
            JA_[anIndex] = adjTr->id();
            anIndex++;
        }
    }
    IA_[trianglesCount] = anIndex;
    MN_ = AN_;

    incompleteCholesky(MN_.data(), JA_.data(), IA_.data(), trianglesCount);
    XN_.fill(0, trianglesCount);
}

void Q3Calc::predictor()
{
    for (int edgeIndex = 0; edgeIndex < mesh_.edges().count(); ++edgeIndex)
    {
        Q3MeshEdge *edge = mesh_.edges().at(edgeIndex);
        edge->processBoundaryVelocity(time_ + tau_ / 2.);
    }

    int iterationsCount = 0;
    qreal maxVelocityDelta;
    while(!abort_ && iterationsCount++ < maxPredictorIterationsCount)
    {
        QVector<QVector2D> tempVelocity(mesh_.triangles().count());
        maxVelocityDelta = 0;

        #pragma omp parallel for
        for (int trInd = 0; trInd < mesh_.triangles().size(); ++trInd)
        {
            Q3MeshTriangle *triangle = mesh_.triangles().at(trInd);
            QVector2D C = triangle->square() / tau_
                          * triangle->correctorVelocity();
            qreal A = triangle->square() / tau_;

            tempVelocity[trInd] = QVector2D(0, 0);

            for (int eInd = 0; eInd < triangle->edges().size(); ++eInd)
            {
                Q3MeshEdge *edge = triangle->edges().at(eInd);
                Q3MeshTriangle *adjTr = triangle->adjacentTriangles().at(eInd);
                QVector2D normal = triangle->normalVectors().at(eInd);

                // Вроде бы всегда так
//                C -= edge->length() * edge->pressure() * normal;

                if (adjTr)
                {
                    qreal dL = triangle->distanceToTriangles().at(eInd);
                    qreal dl = triangle->distancesToEdges().at(eInd);

                    qreal pressure = (dl * adjTr->pressure() +
                                      (dL - dl) * triangle->pressure()) / dL;
                    C -= edge->length() * pressure * normal;

                    qreal vni = (dl * QVector2D::dotProduct(
                                     adjTr->correctorVelocity(), normal)
                                 + (dL - dl) * QVector2D::dotProduct(
                                     triangle->correctorVelocity(), normal)) / dL;

                    qreal tnu = 0;
                    if (monotoneTerm_)
                    {
                        tnu = 0.5 * dL * qAbs(vni) * Re_;
                        QVector2D tAt = QVector2D(adjTr->center()
                                                  - triangle->center());
                        tAt.normalize();
                        qreal cosin = QVector2D::dotProduct(tAt, normal);
                        tnu /= cosin;
                    }

                    qreal B = edge->length() * ((1. + tnu) / Re_ / dL - 0.5 *  vni);

                    tempVelocity[trInd] += B * adjTr->predictorVelocity();
                    A += B;
                }
                else
                {
                    qreal deltaA = \
                            edge->processBoundaryPredictor(Re_, monotoneTerm_,
                                                           tempVelocity[trInd]);
                    C -= edge->length() * triangle->pressure() * normal;
                    A += deltaA;
                }
            }

            tempVelocity[trInd] += C;
            tempVelocity[trInd] /= A;

            qreal velocityDelta = (tempVelocity[trInd]
                                   - triangle->predictorVelocity()).length();
            if (velocityDelta > maxVelocityDelta)
                maxVelocityDelta = velocityDelta;
        }

        for (int trIndex = 0; trIndex < mesh_.triangles().size(); ++trIndex)
        {
            Q3MeshTriangle *triangle = mesh_.triangles().at(trIndex);
            triangle->setPredictorVelocity(tempVelocity[trIndex]);
        }

        if (maxVelocityDelta < maxPredictorError)
            break;
    }

    qDebug() << maxVelocityDelta;
}

void Q3Calc::corrector()
{
    for (int trInd = 0; trInd < mesh_.triangles().count(); ++trInd)
    {
        Q3MeshTriangle *triangle = mesh_.triangles().at(trInd);
        triangle->setPreviousCorrectorVelocity(triangle->correctorVelocity());
        qreal divergence = triangle->divergence(true);
        BN_[trInd] = divergence;
    }

    QTime timer;
    timer.start();
    ConjugateGradient::calculate(AN_.data(), JA_.data(), IA_.data(), XN_.data(),
                                 BN_.data(), MN_.data(), TN_.data(),
                                 mesh_.triangles().count());
    qDebug() << "Cg time:" << timer.elapsed();

    qreal residual = 0;
    for (int trInd = 0; trInd < mesh_.triangles().count(); ++trInd)
    {
        Q3MeshTriangle *triangle = mesh_.triangles().at(trInd);
        QVector2D deltaV(0, 0);

        triangle->setPressure(triangle->pressure() + XN_[trInd]);

        for (int eInd = 0; eInd < triangle->edges().count(); ++eInd)
        {
            Q3MeshEdge *edge = triangle->edges().at(eInd);
            Q3MeshTriangle *adjTr = triangle->adjacentTriangles().at(eInd);
            QVector2D normal = triangle->normalVectors().at(eInd);

            if (adjTr)
            {
                qreal dL = triangle->distanceToTriangles().at(eInd);
                qreal dl = triangle->distancesToEdges().at(eInd);

                qreal dp = (dl * XN_[adjTr->id()] + (dL - dl) * XN_[triangle->id()]) / dL;
                deltaV += tau_ * edge->length() * dp * normal;
            }
            else
            {
                deltaV += tau_ * edge->length() * XN_[triangle->id()] * normal;
            }
        }

        triangle->setCorrectorVelocity(triangle->predictorVelocity() + deltaV / triangle->square());
        if (deltaV.length() > residual)
            residual = deltaV.length();
    }
    residual_ = residual / tau_;
}

void Q3Calc::incompleteCholesky(qreal *AN, int *JA, int *IA, int n)
{
    for (int k = 0; k < n - 1; ++k)
    {
        int d = IA[k];

        if (AN[d] < 0)
            AN[d] *= -1;

        double z = AN[d] = sqrt(AN[d]);

        for (int i = d + 1; i < IA[k + 1]; ++i)
            AN[i] /= z;

        for (int i = d + 1; i < IA[k + 1]; ++i)
        {
            z = AN[i];
            int h = JA[i];
            int g = i;
            for (int j = IA[h] ; j < IA[h + 1]; ++j)
            {
                for ( ; g < IA[k + 1] && JA[g] <= JA[j]; g++)
                {
                    if (JA[g] == JA[j])
                        AN[j] -= z * AN[g];
                }
            }
        }
    }

    int d = IA[n - 1];
    if (AN[d] < 0)
        AN[d] *= -1;

    AN[d] = sqrt(AN[d]);
}

//void Q3Calc::calcFaithfulResidualNS()
//{
//    qreal maxDelta = 0.0;

//    for (int trIndex = 0; trIndex < mesh_.triangles().size(); ++trIndex)
//    {
//        Q3MeshTriangle *triangle = mesh_.triangles().at(trIndex);

//        // Производная по времени
//        QVector2D deltaV = (triangle->correctorVelocity() -
//                            triangle->previousCorrectorVelocity())
//                           / tau_ * triangle->square();

//        for (int edInd = 0; edInd < triangle->edges().count(); ++edInd)
//        {
//            Q3MeshEdge *edge = triangle->edges().at(edInd);
//            Q3MeshTriangle *adjacentTriangle =
//                        triangle->adjacentTriangles().at(edInd);
//            QVector2D normal = triangle->normalVectors().at(edInd);

//            // Давление
//            deltaV += edge->length() * edge->pressure() * normal;

//            if (adjacentTriangle)
//            {
//                // Конвективный поток
//                qreal dL = triangle->distanceToTriangles().at(edInd);
//                qreal dl = triangle->distancesToEdges().at(edInd);

//                qreal vni = (dl * QVector2D::dotProduct(
//                        adjacentTriangle->correctorVelocity(),
//                        normal)
//                    + (dL - dl) * QVector2D::dotProduct(
//                        triangle->correctorVelocity(), normal)) / dL;

//                deltaV += vni * edge->length() * (
//                            triangle->correctorVelocity() +
//                            adjacentTriangle->correctorVelocity())  * 0.5;

//                // Лапласиан
//                deltaV -= edge->length() / Re_ * (
//                            adjacentTriangle->correctorVelocity() -
//                            triangle->correctorVelocity()) / dL;
//            }
//            else
//            {
//                // dL равна dl
//                qreal dl = triangle->distancesToEdges().at(edInd);
//                qreal vni;

//                switch ( edge->boundary()->type()->toEnum() )
//                {
//                case Q3BoundaryType::NoSlipBoundary:
//                    // Конвективный поток вклада не дает
//                    // Лапласиан
//                    // Скорость на границе равна 0

//                    deltaV += edge->length() / Re_ *
//                            triangle->correctorVelocity() / dl;
//                    break;

//                case Q3BoundaryType::FixedVelocity:
//                case Q3BoundaryType::InBoundary:
//                    // Конвективный поток
//                    vni = QVector2D::dotProduct(edge->velocity(), normal);

//                    deltaV += vni * edge->length() *
//                                edge->velocity();

//                    // Лапласиан
//                    deltaV -= edge->length() / Re_ *
//                            (edge->velocity() -
//                             triangle->correctorVelocity()) / dl;

//                    break;

//                case Q3BoundaryType::OutBoundary:
//                    // Конвективный поток
//                    vni = QVector2D::dotProduct(
//                                triangle->correctorVelocity(), normal);

//                    deltaV += vni * edge->length() *
//                                triangle->correctorVelocity();

//                    // Лапласиан вклада не дает
//                    // Производная по нормали должна равняться 0
//                    break;

//                default:
//                    break;
//                }
//            }
//        }

//        deltaV /= triangle->square();

//        if (maxDelta < deltaV.length())
//            maxDelta = deltaV.length();
//    }

//    faithfulResidualNS_ = maxDelta;
//    return;
//}

void Q3Calc::calcFaithfulResidualDiv()
{
    qreal maxDelta = 0.0;

    for (int trInd = 0; trInd < mesh_.triangles().size(); ++trInd)
    {
        Q3MeshTriangle *triangle = mesh_.triangles().at(trInd);

        qreal delta = triangle->divergence(false);

        if (maxDelta < qAbs(delta))
            maxDelta = qAbs(delta);
   }

   faithfulResidualDiv_ = maxDelta;
}

void Q3Calc::setMonotoneTerm(bool monotoneTerm)
{
    monotoneTerm_ = monotoneTerm;
}


void Q3Calc::setRe(const qreal &Re)
{
    Re_ = Re;
}

void Q3Calc::setTau(const qreal &tau)
{
    tau_ = tau;
}

void Q3Calc::setBadTriangleFix(bool badTriangleFix)
{
    badTriangleFix_ = badTriangleFix;
}

void Q3Calc::abort()
{
    abort_ = true;
}

void Q3Calc::reset()
{
    time_ = 0;
    abort_ = true;
    wait();
    started_ = false;
    residual_ = 0;
    calcTime_ = 0;
    emit calcStepEnded(time_);
}

QString Q3Calc::info()
{
    if (started_)
        calcTime_ += calcTimer_.restart();

    QString out;
    QTextStream stream(&out);
    if (started_)
    {
        stream << trUtf8("Невязка предиктор-корректор: ")
               << QString::number(residual_) << "\n"
               << trUtf8("Настоящая невязка Навье-Стокс: ")
               << QString::number(faithfulResidualNS_) << "\n"
               << trUtf8("Настоящая невязка Дивергенция: ")
               << QString::number(faithfulResidualDiv_) << "\n"
               << trUtf8("Время: ")
               << QString::number(time_) << "\n"
               << trUtf8("Время расчета: ")
               << QString::number(calcTime_ / 1000.) << "\n";
    }
    else
        stream << trUtf8("Инфоормация о расчете\n");
    return out;
}
