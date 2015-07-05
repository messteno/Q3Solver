#include <cstdio>
#include <string>
#include "exprtk.h"

typedef exprtk::symbol_table<double>  symbolTableT;
typedef exprtk::expression<double>     expressionT;
typedef exprtk::parser<double>             parserT;


class Q3XYExpression
{
public:
    Q3XYExpression();
    ~Q3XYExpression();
    bool setString(std::string str);
    double getValue(double x, double y, double t);
	
private:
	
    symbolTableT symbolTable_;
    expressionT expression_;
    parserT parser_;

	double x_;
	double y_;
    double t_;
    std::string sExpr_;
};

