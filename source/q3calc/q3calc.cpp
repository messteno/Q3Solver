#include <QDebug>
#include <QTime>

#include <qmath.h>

#include "q3calc.h"
#include "conjugategradient.h"
#include "bicgstablinearsolver.h"
#include "preconditioner.h"
#include "naturalneigbourinterpolation.h"

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
//    abort_ = true;
    while(!abort_)
    {
        // QTime timer;

        // timer.start();
        predictor();
        // qDebug() << "Predictor time:" << timer.elapsed();

        // timer.start();
        corrector();
        // qDebug() << "Corrector time: " << timer.elapsed();

        calcFaithfulResidualNS();
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

//        break;
    }
    calcTime_ += calcTimer_.elapsed();
}

void Q3Calc::prepare()
{
    // TODO:
    int edgesCount = mesh_.edges().count();

    AN_.clear();
    JA_.clear();
    IA_.clear();
    BN_.clear();
    XN_.clear();
    TN_.clear();

    AN_.fill(0, 3 * edgesCount);
    JA_.fill(0, 3 * edgesCount);
    IA_.fill(0, edgesCount + 1);
    BN_.fill(0, edgesCount);
    XN_.fill(0, edgesCount);
    TN_.fill(0, 4 * edgesCount);

    int anIndex = 0;
    for (int edgeIndex = 0; edgeIndex < edgesCount; ++edgeIndex)
    {
        Q3MeshEdge *edge = mesh_.edges().at(edgeIndex);

        // Возможно вынести в соответствующий boundarytype-класс
        if (edge->boundary())
        {
            if (edge->boundary()->type()->toEnum() == Q3BoundaryType::OutBoundary)
            {
                AN_[anIndex] = 1.;
                JA_[anIndex] = edgeIndex;
                IA_[edgeIndex] = anIndex;
                anIndex++;
                continue;
            }
        }

        for (int cotIndex = 0; cotIndex < edge->adjacentCotangents().count();
             ++cotIndex)
        {
            AN_[anIndex] += edge->adjacentCotangents().at(cotIndex);
        }

        AN_[anIndex] *= 2. * tau_;
        JA_[anIndex] = edgeIndex;
        IA_[edgeIndex] = anIndex;
        anIndex++;

//        Q_ASSERT(edge->adjacentEdges().count() == (edge->boundary() ? 2 : 4));

        for (int adjInd = 0; adjInd < edge->adjacentEdges().count(); ++adjInd)
        {
            Q3MeshEdge *adjEdge = edge->adjacentEdges().at(adjInd);
            if (adjEdge->id() > edge->id())
            {
                AN_[anIndex] = -2. * tau_ * edge->adjacentCotangents().at(adjInd);
                JA_[anIndex] = adjEdge->id();
                anIndex++;
            }
        }
    }
    IA_[edgesCount] = anIndex;
    MN_ = AN_;

    incompleteCholesky(MN_.data(), JA_.data(), IA_.data(), edgesCount);
    XN_.fill(0, mesh_.edges().count());

//    QVector<QVector3D> uValues;
//    QVector<QVector3D> vValues;
//    QVector<QVector3D> pValues;

//    QFile fu("/home/mesteno/up.txt");
//    if (fu.open(QFile::ReadOnly))
//    {
//        QTextStream in(&fu);
//        while (!in.atEnd())
//        {
//            qreal x, y, u, v, p;
//            in >> x >> y >> u >> v >> p;
//            if (in.atEnd())
//                break;
//            uValues.append(QVector3D(x, y, u));
//            vValues.append(QVector3D(x, y, v));
//            pValues.append(QVector3D(x, y, p));
//        }
//        fu.close();
//    }

//    foreach (Q3MeshNode *node, mesh_.nodes())
//    {
//        if (node->boundary())
//        {
//            qreal x = node->x();
//            qreal y = node->y();
//            if (node->x() == 0)
//                uValues.append(QVector3D(x, y, qMax(0., 24 * (1 - y) * (y - 0.5))));
//            else if (node->x() == 10)
//                uValues.append(QVector3D(x, y, (1 - y) * y * 3));
//            else
//                uValues.append(QVector3D(x, y, 0));
//            vValues.append(QVector3D(x, y, 0));

//            qreal dmin = 1;
//            QVector3D minv;
//            foreach (QVector3D pv, pValues)
//            {
//               QVector2D vec(pv.x(), pv.y());
//               vec -= QVector2D(x, y);
//               if (vec.length() < dmin)
//               {
//                   dmin = vec.length();
//                   minv = pv;
//               }
//            }
//            pValues.append(QVector3D(x, y, minv.z()));
//        }
//    }

//    foreach (Q3MeshEdge *edge, mesh_.edges())
//    {
//        if (edge->boundary())
//        {
//            qreal y = edge->center().y();
//            if (edge->boundary()->type()->toEnum() == Q3BoundaryType::InBoundary)
//                edge->setVelocity(QVector2D(24 * (1 - y) * (y - 0.5), 0));
//            else if (edge->boundary()->type()->toEnum() == Q3BoundaryType::OutBoundary)
//                edge->setVelocity(QVector2D(3 * (1 - y) * y, 0));
//        }
//    }

//    NaturalNeigbourInterpolation uInterp(uValues);
//    NaturalNeigbourInterpolation vInterp(vValues);
//    foreach (Q3MeshTriangle *triangle, mesh_.triangles())
//    {
//        qreal u = uInterp.interpolateToPoint(triangle->center());
//        qreal v = vInterp.interpolateToPoint(triangle->center());

//        triangle->setPredictorVelocity(QVector2D(u, v));
//        triangle->setCorrectorVelocity(QVector2D(u, v));
//        triangle->setPreviousCorrectorVelocity(QVector2D(u, v));
//    }

//    NaturalNeigbourInterpolation pInterp(pValues);
//    foreach (Q3MeshEdge *edge, mesh_.edges())
//    {
//        qreal p = pInterp.interpolateToPoint(edge->center());
//        edge->setPressure(p);
//        edge->processBoundaryVelocity(0);
//    }

//    calcFaithfulResidualNS();
//    calcFaithfulResidualDiv();
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
                C -= edge->length() * edge->pressure() * normal;

                if (adjTr)
                {
                    qreal dL = triangle->distanceToTriangles().at(eInd);
                    qreal dl = triangle->distancesToEdges().at(eInd);

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

                    qreal B = edge->length() * ((1. + tnu) / Re_ / dL - dl * vni / dL);
                    qreal BB = edge->length() * ((1. + tnu) / Re_ / dL + (dL - dl) * vni / dL);

                    tempVelocity[trInd] += B * adjTr->predictorVelocity();
                    A += BB;
                }
                else
                {
                    qreal deltaA = \
                            edge->processBoundaryPredictor(Re_, monotoneTerm_,
                                                           tempVelocity[trInd]);
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

    for (int trIndex = 0; trIndex < mesh_.triangles().size(); ++trIndex)
    {
        Q3MeshTriangle *triangle = mesh_.triangles().at(trIndex);
        triangle->setPreviousCorrectorVelocity(triangle->correctorVelocity());
    }

    qreal flow = 0;
    for (int edgeIndex = 0; edgeIndex < mesh_.edges().size(); ++edgeIndex)
    {
        Q3MeshEdge *edge = mesh_.edges().at(edgeIndex);
        flow += edge->processBoundaryFlow();
    }
    qDebug() << "Flow: " << flow;

    int nullPrEInd = -1;
    for (int eInd = 0; eInd < mesh_.edges().size(); ++eInd)
    {
        Q3MeshEdge *edge = mesh_.edges().at(eInd);
        Q3MeshTriangle *tr0 = edge->adjacentTriangles().at(0);
        int trEdgeIndex = tr0->edges().indexOf(edge);
        QVector2D normal = tr0->normalVectors().at(trEdgeIndex);

        if (nullPrEInd == -1 && edge->boundary()
            && edge->boundary()->type()->toEnum() == Q3BoundaryType::OutBoundary)
        {
            nullPrEInd = eInd;
        }

        if (edge->adjacentTriangles().size() == 2)
        {
            Q3MeshTriangle *tr1 = edge->adjacentTriangles().at(1);

            qreal bn0 = QVector2D::dotProduct(tr1->predictorVelocity(), normal)
                        - QVector2D::dotProduct(tr0->predictorVelocity(), normal);
            if (badTriangleFix_)
            {
                QVector2D tAt = QVector2D(tr0->center() - tr1->center());
                qreal cosin = qAbs(QVector2D::dotProduct(tAt, normal))
                              / tAt.length();
                bn0 /= cosin;
            }
            bn0 *= -edge->length();

            qreal dL = tr0->distanceToTriangles().at(trEdgeIndex);
            qreal dl = tr0->distancesToEdges().at(trEdgeIndex);
            qreal bn1 = (tr0->divergence(true) * tr0->square() + tr1->divergence(true) * tr1->square())
                       / (tr0->square() + tr1->square());
//            qreal bn1 = (tr0->divergence(true) * (dL - dl) + tr1->divergence(true) * dl) / dL;
            bn1 *= -edge->adjacentSquare();

            BN_[eInd] = bn1;
        }
        else
        {
            BN_[eInd] = edge->processBoundaryCorrector();
            BN_[eInd] *= -edge->length();

//            qreal bn1 = tr0->divergence(true);
//            bn1 *= -edge->adjacentSquare();
//            BN_[eInd] = bn1;
        }
//        BN_[eInd] += edge->adjacentSquare() * flow / mesh_.square();
    }

    QTime timer;
    timer.start();
//    XN_.fill(0, mesh_.edges().count());
    ConjugateGradient::calculate(AN_.data(), JA_.data(), IA_.data(), XN_.data(),
                                 BN_.data(), MN_.data(), TN_.data(),
                                 mesh_.edges().count());
    qDebug() << "Cg time:" << timer.elapsed();

    for (int eInd = 0; eInd < mesh_.edges().count(); ++eInd)
    {
        Q3MeshEdge *edge = mesh_.edges().at(eInd);
        if (!edge->boundary()
            || edge->boundary()->type()->toEnum() != Q3BoundaryType::OutBoundary)
        {
            edge->setPressure(edge->pressure() + XN_[eInd] - XN_[nullPrEInd]);
        }
    }

    qreal residual = 0;
    for (int trInd = 0; trInd < mesh_.triangles().count(); ++trInd)
    {
        Q3MeshTriangle *triangle = mesh_.triangles().at(trInd);

        QVector2D sum(0, 0);
        for (int eInd = 0; eInd < triangle->edges().count(); ++eInd)
        {
            Q3MeshEdge *edge = triangle->edges().at(eInd);
            sum += edge->length() * XN_[edge->id()]
                    * triangle->normalVectors().at(eInd);
        }
        QVector2D prevVelocity = triangle->correctorVelocity();
        triangle->setCorrectorVelocity(triangle->predictorVelocity()
                                       - tau_ * sum / triangle->square());
        qreal deltaV = (prevVelocity - triangle->correctorVelocity()).length();
        if (deltaV > residual)
            residual = deltaV;
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

void Q3Calc::calcFaithfulResidualNS()
{
    qreal maxDelta = 0.0;

    for (int trIndex = 0; trIndex < mesh_.triangles().size(); ++trIndex)
    {
        Q3MeshTriangle *triangle = mesh_.triangles().at(trIndex);

        // Производная по времени
        QVector2D deltaV = (triangle->correctorVelocity() -
                            triangle->previousCorrectorVelocity())
                           / tau_ * triangle->square();
        for (int edInd = 0; edInd < triangle->edges().count(); ++edInd)
        {
            Q3MeshEdge *edge = triangle->edges().at(edInd);
            Q3MeshTriangle *adjacentTriangle =
                        triangle->adjacentTriangles().at(edInd);
            QVector2D normal = triangle->normalVectors().at(edInd);

            // Давление
            deltaV += edge->length() * edge->pressure() * normal;

            if (adjacentTriangle)
            {
                // Конвективный поток
                qreal dL = triangle->distanceToTriangles().at(edInd);
                qreal dl = triangle->distancesToEdges().at(edInd);

                qreal vni = (dl * QVector2D::dotProduct(
                        adjacentTriangle->correctorVelocity(),
                        normal)
                    + (dL - dl) * QVector2D::dotProduct(
                        triangle->correctorVelocity(), normal)) / dL;

                deltaV += vni * edge->length() * (
                            (dL - dl) * triangle->correctorVelocity() +
                            dl * adjacentTriangle->correctorVelocity()) / dL;
//                deltaV += vni * edge->length() * (
//                            (dL - dl) * triangle->correctorVelocity() +
//                            dl * adjacentTriangle->correctorVelocity()) / dL;

                // Лапласиан
                deltaV -= edge->length() / Re_ * (
                            adjacentTriangle->correctorVelocity() -
                            triangle->correctorVelocity()) / dL;
            }
            else
            {
                // dL равна dl
                qreal dl = triangle->distancesToEdges().at(edInd);
                qreal vni;

                switch ( edge->boundary()->type()->toEnum() )
                {
                case Q3BoundaryType::NoSlipBoundary:
                    // Конвективный поток вклада не дает
                    // Лапласиан
                    // Скорость на границе равна 0

                    deltaV += edge->length() / Re_ *
                            triangle->correctorVelocity() / dl;
                    break;

                case Q3BoundaryType::FixedVelocity:
                case Q3BoundaryType::InBoundary:
                    // Конвективный поток
                    vni = QVector2D::dotProduct(edge->velocity(), normal);

                    deltaV += vni * edge->length() *
                                edge->velocity();

                    // Лапласиан
                    deltaV -= edge->length() / Re_ *
                            (edge->velocity() -
                             triangle->correctorVelocity()) / dl;
                    break;

                case Q3BoundaryType::OutBoundary:
                    // Конвективный поток
                    vni = QVector2D::dotProduct(
                                triangle->correctorVelocity(), normal);

                    deltaV += vni * edge->length() *
                                triangle->correctorVelocity();

                    // Лапласиан вклада не дает
                    // Производная по нормали должна равняться 0
                    break;

                default:
                    break;
                }
            }
        }

        deltaV /= triangle->square();
        triangle->setResidual(deltaV.length());

        if (maxDelta < deltaV.length())
            maxDelta = deltaV.length();
    }

//    qDebug() << maxDelta;

    faithfulResidualNS_ = maxDelta;
    return;
}

void Q3Calc::calcFaithfulResidualDiv()
{
    qreal maxDelta = 0.0;

    for (int trIndex = 0; trIndex < mesh_.triangles().size(); ++trIndex)
    {
        Q3MeshTriangle *triangle = mesh_.triangles().at(trIndex);
        qreal delta = triangle->divergence(false);
        triangle->setResidual(qAbs(delta));

        if (maxDelta < qAbs(delta))
            maxDelta = qAbs(delta);
   }

   faithfulResidualDiv_ = maxDelta;
   return;
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
