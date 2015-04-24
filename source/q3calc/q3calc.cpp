#include <QDebug>
#include <qmath.h>

#include "q3calc.h"
#include "conjugategradient.h"

const int Q3Calc::maxPredictorIterationsCount = 10000;
const qreal Q3Calc::maxPredictorError = 1e-9;

// Возможно переделать геттеры в треугольниках на ссылки

Q3Calc::Q3Calc(Q3Mesh *mesh, qreal tau, qreal Re, QObject *parent) :
    QThread(parent),
    mesh_(mesh),
    tau_(tau),
    Re_(Re),
    residual_(0)
{
}

Q3Calc::~Q3Calc()
{
    abort_ = true;
    wait();
}

void Q3Calc::run()
{
    abort_ = false;
    prepare();
    while(!abort_)
    {
        predictor();
        corrector();
        emit updateInfo();
    }
}

void Q3Calc::prepare()
{
    // TODO:
    int edgesCount = mesh_->edges().count();

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
    int boundaryCount = 0;
    for (int edgeIndex = 0; edgeIndex < edgesCount; ++edgeIndex)
    {
        Q3MeshEdge *edge = mesh_->edges().at(edgeIndex);

        edge->processBoundaryVelocity();

        // Возможно вынести в соответствующий boundarytype-класс
        if (edge->boundary())
        {
            boundaryCount++;
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

        for (int adjEdgeIndex = 0; adjEdgeIndex < edge->adjacentEdges().count();
             ++adjEdgeIndex)
        {
            Q3MeshEdge *adjEdge = edge->adjacentEdges().at(adjEdgeIndex);
            if (adjEdge->id() > edge->id())
            {
                AN_[anIndex] = - 2. * tau_
                               * edge->adjacentCotangents().at(adjEdgeIndex);
                JA_[anIndex] = adjEdge->id();
                anIndex++;
            }
        }
    }
    IA_[edgesCount] = anIndex;
    MN_ = AN_;

    incompleteCholesky(MN_.data(), JA_.data(), IA_.data(), edgesCount);
}

void Q3Calc::predictor()
{
    // TODO:
    foreach (Q3MeshTriangle *triangle, mesh_->triangles())
        triangle->setPredictorVelocity(triangle->correctorVelocity());

    int iterationsCount = 0;
    while(iterationsCount++ < maxPredictorIterationsCount)
    {
        qreal maxVelocityDelta = 0;

        for (int trIndex = 0; trIndex < mesh_->triangles().size(); ++trIndex)
        {
            Q3MeshTriangle *triangle = mesh_->triangles().at(trIndex);
            QVector2D C = triangle->square() / tau_
                          * triangle->correctorVelocity();
            triangle->setTempVelocity(QVector2D(0, 0));
            qreal A = 0;

            for (int edgeIndex = 0; edgeIndex < triangle->edges().size();
                 ++edgeIndex)
            {
                Q3MeshEdge *edge = triangle->edges().at(edgeIndex);
                Q3MeshTriangle *adjacentTriangle =
                        triangle->adjacentTriangles().at(edgeIndex);
                QVector2D normal = triangle->normalVectors().at(edgeIndex);

                // Вроде бы всегда так
                C -= edge->length() * edge->preassure() * normal;

                if (adjacentTriangle)
                {
                    qreal dL = triangle->distanceToTriangles().at(edgeIndex);
                    qreal dl = triangle->distancesToEdges().at(edgeIndex);

                    qreal vni = dl * QVector2D::dotProduct(
                                    adjacentTriangle->correctorVelocity(),
                                    normal);
                    vni += (dL - dl) * QVector2D::dotProduct(
                               triangle->correctorVelocity(),
                               normal);
                    vni /= dL;

                    qreal tnu = 0.5 * dL * qAbs(vni) * Re_;
                    QVector2D tAt = QVector2D(adjacentTriangle->center()
                                              - triangle->center());
                    qreal cosin = QVector2D::dotProduct(tAt, normal)
                                  / tAt.length();
                    tnu /= cosin;

                    qreal B = edge->length() / Re_ / dL
                              * (1 + tnu - 0.5 * dL * vni * Re_);

                    triangle->setTempVelocity(triangle->tempVelocity() + B
                                              * triangle->correctorVelocity());
                    A += B;
                }
                else
                {
                    qreal deltaA = edge->processBoundaryPredictor(Re_);
                    A += deltaA;
                }
            }

            A += triangle->square() / tau_;
            triangle->setTempVelocity(triangle->tempVelocity() + C / A);

            qreal velocityDelta = (triangle->tempVelocity()
                                   - triangle->predictorVelocity()).length();
            if (velocityDelta > maxVelocityDelta)
                maxVelocityDelta = velocityDelta;
        }

        for (int trIndex = 0; trIndex < mesh_->triangles().size(); ++trIndex)
        {
            Q3MeshTriangle *triangle = mesh_->triangles().at(trIndex);
            triangle->setPredictorVelocity(triangle->tempVelocity());
        }

        if (maxVelocityDelta < maxPredictorError)
            break;
    }
}

void Q3Calc::corrector()
{
    qreal flow = 0;
    for (int edgeIndex = 0; edgeIndex < mesh_->edges().size(); ++edgeIndex)
    {
        Q3MeshEdge *edge = mesh_->edges().at(edgeIndex);
        flow += edge->processBoundaryFlow();
    }

    qreal allSquare = 0;
    for (int trIndex = 0; trIndex < mesh_->triangles().size(); ++trIndex)
    {
        Q3MeshTriangle *triangle = mesh_->triangles().at(trIndex);
        allSquare += triangle->square();
    }

    for (int edgeIndex = 0; edgeIndex < mesh_->edges().size(); ++edgeIndex)
    {
        Q3MeshEdge *edge = mesh_->edges().at(edgeIndex);
        Q3MeshTriangle *tr0 = edge->adjacentTriangles().at(0);
        int trEdgeIndex = tr0->edges().indexOf(edge);
        QVector2D normal = tr0->normalVectors().at(trEdgeIndex);

        if (edge->adjacentTriangles().size() == 2)
        {
            Q3MeshTriangle *tr1 = edge->adjacentTriangles().at(1);

            BN_[edgeIndex] = QVector2D::dotProduct(tr1->predictorVelocity(),
                                                   normal)
                             - QVector2D::dotProduct(tr0->predictorVelocity(),
                                                     normal);
            QVector2D tAt = QVector2D(tr0->center() - tr1->center());
            qreal cosin = qAbs(QVector2D::dotProduct(tAt, normal) / tAt.length());
            BN_[edgeIndex] /= cosin;
        }
        else
        {
            BN_[edgeIndex] = edge->processBoundaryCorrector();
        }

        BN_[edgeIndex] *= - edge->length();
        BN_[edgeIndex] += edge->adjacentSquare() * flow / allSquare;
    }

    XN_.fill(0, mesh_->edges().count());
    ConjugateGradient::calculate(AN_.data(), JA_.data(), IA_.data(), XN_.data(),
                                 BN_.data(), MN_.data(), TN_.data(),
                                 mesh_->edges().count());

    for (int edgeIndex = 0; edgeIndex < mesh_->edges().count(); ++edgeIndex)
    {
        Q3MeshEdge *edge = mesh_->edges().at(edgeIndex);
        edge->setPreassure(edge->preassure() + XN_[edgeIndex]);
    }

    qreal residual = 0;
    for (int trIndex = 0; trIndex < mesh_->triangles().count(); ++trIndex)
    {
        Q3MeshTriangle *triangle = mesh_->triangles().at(trIndex);

        QVector2D sum(0, 0);
        for (int edgeIndex = 0; edgeIndex < triangle->edges().count();
             ++edgeIndex)
        {
            Q3MeshEdge *edge = triangle->edges().at(edgeIndex);
            sum += edge->length() * XN_[edge->id()]
                    * triangle->normalVectors().at(edgeIndex);
        }
        QVector2D prevVelocity = triangle->correctorVelocity();
        triangle->setCorrectorVelocity(triangle->predictorVelocity()
                                       - tau_ * sum / triangle->square());
        qreal deltaV = (prevVelocity - triangle->correctorVelocity()).length();
        if (deltaV > residual)
            residual = deltaV;
    }
    residual_ = residual / tau_;
    qDebug() << residual_;
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

void Q3Calc::abort()
{
    abort_ = true;
}

QString Q3Calc::info()
{
    QString out;
    QTextStream stream(&out);
    stream << tr("Невязка: ") << QString::number(residual_) << "\n";
    return out;
}
