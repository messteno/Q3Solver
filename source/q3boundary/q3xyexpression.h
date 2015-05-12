#include <cstdio>
#include <string>
#include "exprtk.h"

typedef exprtk::symbol_table<double> symbol_table_t;
typedef exprtk::expression<double>     expression_t;
typedef exprtk::parser<double>             parser_t;


class q3XYExpression
{
public:
	q3XYExpression ();
	~q3XYExpression ();
	int setString (std::string str);
	double getValue (double x, double y);
	
private:
	
	symbol_table_t symbol_table;
	expression_t expression;
	parser_t parser;

	double x_;
	double y_;
    std::string s_expr;
};

