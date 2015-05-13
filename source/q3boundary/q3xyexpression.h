#include <cstdio>
#include <string>
#include "exprtk.h"

typedef exprtk::symbol_table<double>  symbolTableT;
typedef exprtk::expression<double>     expressionT;
typedef exprtk::parser<double>             parserT;


class q3XYExpression
{
public:
	q3XYExpression ();
	~q3XYExpression ();
    bool setString (std::string str);
	double getValue (double x, double y);
	
private:
	
    symbolTableT symbolTable_;
    expressionT expression_;
    parserT parser_;

	double x_;
	double y_;
    std::string sExpr_;
};

