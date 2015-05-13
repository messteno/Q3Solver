#include "q3xyexpression.h"

q3XYExpression::q3XYExpression ()
{
   symbolTable_.add_variable("x", x_);
   symbolTable_.add_variable("y", y_);
   symbolTable_.add_constants();

   expression_.register_symbol_table(symbolTable_);
   sExpr_ = "0";

   parser_.compile(sExpr_, expression_);
}

q3XYExpression::~q3XYExpression ()
{

}

bool q3XYExpression::setString (std::string str)
{
    sExpr_	= str;

    if (!parser_.compile(sExpr_, expression_))
	{
        sExpr_ = "0";
        parser_.compile(sExpr_, expression_);
        return false;
	}

    return true;
}


double q3XYExpression::getValue (double x, double y)
{
	x_ = x;
	y_ = y;

    return expression_.value();
}

