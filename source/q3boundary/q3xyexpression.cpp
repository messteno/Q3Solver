#include "q3xyexpression.h"

q3XYExpression::q3XYExpression ()
{
   symbol_table.add_variable("x", x_);
   symbol_table.add_variable("y", y_);
   symbol_table.add_constants();

   expression.register_symbol_table(symbol_table);
   s_expr = "0";

   parser.compile(s_expr, expression);
}

q3XYExpression::~q3XYExpression ()
{

}

int q3XYExpression::setString (std::string str)
{
	s_expr	= str;

	if (!parser.compile(s_expr, expression))
	{
		s_expr = "0";
		parser.compile(s_expr, expression);
		return 0;
	}

	return 1;
}


double q3XYExpression::getValue (double x, double y)
{
	x_ = x;
	y_ = y;

	return expression.value();
}

