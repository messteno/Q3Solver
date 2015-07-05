#include "q3xyexpression.h"

Q3XYExpression::Q3XYExpression() :
    x_(0),
    y_(0),
    t_(0)
{
    symbolTable_.add_variable("x", x_);
    symbolTable_.add_variable("y", y_);
    symbolTable_.add_variable("t", t_);
    symbolTable_.add_constants();

    expression_.register_symbol_table(symbolTable_);
    sExpr_ = "0";

    parser_.compile(sExpr_, expression_);
}

Q3XYExpression::~Q3XYExpression ()
{

}

bool Q3XYExpression::setString(std::string str)
{
    sExpr_ = str;

    if (!parser_.compile(sExpr_, expression_))
    {
        sExpr_ = "0";
        parser_.compile(sExpr_, expression_);
        return false;
    }

    return true;
}


double Q3XYExpression::getValue(double x, double y, double t)
{
    x_ = x;
    y_ = y;
    t_ = t;

    return expression_.value();
}

