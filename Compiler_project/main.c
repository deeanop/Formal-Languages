#include <stdio.h>
#include "utils.h"
#include "lexer.h"
#include "parser.h"
#include "ad.h"
#include "at.h"
//#include "vm.h"
//#include "gc.h"
int main()
{
	char *program = loadFile("testat.c");
	Token *tokenList = tokenize(program);
	showTokens(tokenList);
	pushDomain();
	//vmInit();
	parse(tokenList);
	showDomain(symTable, "global");
	//Instr *testCode=genDoubleTestProgram();
	//run(testCode);
	dropDomain();
	return 0;
}
