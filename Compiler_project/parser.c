#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include "parser.h"

Token *iTk;		// the iterator in the tokens list
Token *consumedTk;		// the last consumed token

bool varDef();
bool typeBase();
bool arrayDecl();
bool fnDef();
bool fnParam();
bool stm();
bool stmCompound();
bool expr();
bool exprRel();
bool exprEq();
bool exprAdd();
bool exprMul();
bool exprCast();
bool exprUnary();
bool exprPostfix();
bool exprPrimary();
bool exprAnd();
bool exprOr();


void tkerr(const char *fmt,...){
	fprintf(stderr,"error in line %d: ",iTk->line);
	va_list va;
	va_start(va,fmt);
	vfprintf(stderr,fmt,va);
	va_end(va);
	fprintf(stderr,"\n");
	exit(EXIT_FAILURE);
	}
	
bool consume(int code){
	if(iTk->code==code){
		consumedTk=iTk;
		iTk=iTk->next;
		return true;
		}
	return false;
}

// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
bool typeBase(){
	if(consume(STRUCT)){
		if(consume(ID)){
			return true;
		}
	}
	if(consume(TYPE_INT))
			return true;
	if(consume(TYPE_DOUBLE))
			return true;
	if(consume(TYPE_CHAR))
			return true;
	return false;
}
	
//arrayDecl: LBRACKET INT? RBRACKET
bool arrayDecl(){
	Token *start = iTk;
	if(consume(LBRACKET)){
		consume(INT);
		if(consume(RBRACKET)){
			return true;
		}else tkerr("Lipseste ] dupa dimensiunea array-ului\n");
	}
	iTk = start;
	return false;
}

//varDef: typeBase ID arrayDecl? SEMICOLON
bool varDef(){
	Token *start = iTk;
	if(typeBase()){
		if(consume(ID)){
			arrayDecl();
			if(consume(SEMICOLON)){
				return true;
			}else tkerr("Lipseste ; dupa declaratia de variabila\n");
		}else tkerr("Lipseste numele variabilei\n");
	}
	iTk=start;
	return false;
}

//structDef: STRUCT ID LACC varDef* RACC SEMICOLON
bool structDef(){
	Token *start = iTk;
	if(consume(STRUCT)){
		if(consume(ID)){
			if(consume(LACC)){
				while(varDef()){}
				if(consume(RACC)){
					if(consume(SEMICOLON)){
						return true;
					}else tkerr("Lipseste ; dupa struct\n");
				}else tkerr("Lipseste } dupa struct\n");
			}else if(!consume(LACC) && varDef()) tkerr("Lipseste { dupa struct\n"); //modificare adusa pentru a se evita intrarea in eroarea "Lipseste { dupa struct"
		}else tkerr("Lipseste numele structurii\n");
	}
	iTk = start;
	return false;
}

//fnParam: typeBase ID arrayDecl?
bool fnParam(){
	Token *start = iTk;
	if(typeBase()){
		if(consume(ID)){
			arrayDecl();
			return true;
		}else tkerr("Lipseste numele parametrului\n");
	}
	iTk = start;
	return false;
}

//exprRel: exprRel(LESS | LESSEQ | GREATER | GREATEREQ) exprAdd | exprAdd
//exprRel_1: (LESS | LESSEQ | GREATER | GREATEREQ) exprAdd exprRel_1 | epsilon
bool exprRel_1(){
	if(consume(LESS) || consume(LESSEQ) || consume(GREATER) || consume(GREATEREQ)){
		if(exprAdd()){
			if(exprRel_1()){
				return true;
			}else tkerr("Lipseste expresia relationala.\n");
		}else tkerr("Lipseste expresia adunarii.\n");
	}
	return true;  
}

//exprRel: exprAdd exprRel_1
bool exprRel(){
	Token *start = iTk;
	if(exprAdd()){
		if(exprRel_1()){
			return true;
		}else tkerr("Lipseste expresia relationala.\n");
	}
	iTk = start;
	return false;
}

//exprEq: exprEq (EQUAL | NOTEQ) exprRel | exprRel
//exprEq_1: (EQUAL| NOTEQ) exprRel exprEq_1 | epsilon
bool exprEq_1(){
	if(consume(EQUAL) || consume(NOTEQ)){
		if(exprRel()){
			if(exprEq_1()){
				return true;
			}else tkerr("Lipseste expresia relationala, precedata de == sau !=.\n");
		}else tkerr("Lipseste expresia de dupa == sau !=.\n");
	}
	return true;
}

//exprEq: exprRel exprEq_1
bool exprEq(){
	Token *start = iTk;
	if(exprRel()){
		if(exprEq_1()){
			return true;
		}else tkerr("Lipseste expresia relationala, precedata de == sau !=.\n"); 
	}
	iTk = start;
	return false;
}

//exprAnd: exprAnd AND exprEq | exprEq
//exprAnd_1: AND exprEq exprAnd_1| epsilon
bool exprAnd_1(){
	if(consume(AND)){
		if(exprEq()){
			if(exprAnd_1()){ 
				return true;
			}else tkerr("Lipseste expresia de dupa &&.\n");
		}else tkerr("Lipseste expresia de dupa &&.\n");
	}
	return true;
}

//exprAnd: exprEq exprAnd_1
bool exprAnd(){
	Token *start = iTk;
	if(exprEq()){
		if(exprAnd_1()){
			return true;
		}else tkerr("Lipseste expresia de dupa &&.\n");
	}
	iTk = start;
	return false;
}

//exprOr: exprOr OR exprAnd | exprAnd
//exprOr_1: OR exprAnd exprOr_1 | epsilon
bool exprOr_1(){
	if(consume(OR)){
		if(exprAnd()){
			if(exprOr_1()){
				return true;
			}else tkerr("Lipseste expresia de dupa ||.\n");
		}else tkerr("Lipseste expresia de dupa ||.\n");
	}
	return true;
}

//exprOr: exprAnd exprOr_1
bool exprOr(){
	Token *start = iTk;
	if(exprAnd()){
		if(exprOr_1()){
			return true;
		}else tkerr("Lipseste expresia de dupa ||.\n");
	}
	iTk = start;
	return false;
}

//exprAssign: exprUnary ASSIGN exprAssign | exprOr
bool exprAssign(){
    Token *start = iTk;
	if(exprUnary()){
		if(consume(ASSIGN)){
			if(exprAssign()){
				return true;
			}else tkerr("Lipseste expresia de dupa =\n");
		}
	}
    iTk = start;
	if(exprOr()){
		return true;
	}
    iTk = start;
	return false;
}

//expr: exprAssign
bool expr(){
	if(exprAssign()){
		return true;
	}
	return false;
}

//exprPrimary: ID ( LPAR ( expr ( COMMA expr )* )? RPAR )? | INT | DOUBLE | CHAR | STRING | LPAR expr RPAR
bool exprPrimary(){
	if(consume(ID)){
		if(consume(LPAR)){
			if(expr()){
				while(consume(COMMA)){
					if(!expr()){
						tkerr("Lipseste expresia de dupa virgula.\n");  
					}
				}
			}
			if(consume(RPAR)){
				return true;
			}else tkerr("Lipseste ) dupa argumentele functiei.\n");
		}
		return true;
	}
	if(consume(INT)){
		return true;
	}
	if(consume(DOUBLE)){
		return true;
	}
	if(consume(CHAR)){
		return true;
	}
	if(consume(STRING)){
		return true;
	}
	if(consume(LPAR)){
		if(expr() || typeBase()){
			if(consume(RPAR)){
				return true;
			}else tkerr("Lipseste ) dupa expresia din paranteza\n");
		}else tkerr("Lipseste expresia din paranteza\n");
	}
	return false;
}

//exprPostfix: exprPostfix LBRACKET expr RBRACKET | exprPostfix DOT ID | exprPrimary
//exprPostfix_1: LBRACKET expr RBRACKET exprPostfix_1 | DOT ID exprPostfix_1 | epsilon
bool exprPostfix_1(){
	if(consume(LBRACKET)){
		if(expr()){
			if(consume(RBRACKET)){
				if(exprPostfix_1()){
					return true;
				}else tkerr("Lipseste expresia de indexare.\n"); 
			}else tkerr("Lipseste ] dupa expresia din paranteza.\n");
		}else tkerr("Lipseste expresia din paranteza.\n");
	}
	if(consume(DOT)){
		if(consume(ID)){
			if(exprPostfix_1()){
				return true;
			}else tkerr("Lipseste expresia de indexare.\n");
		}else tkerr("Lipseste identificatorul.\n");
	}
	return true;
}

//exprPostfix: exprPrimary exprPostfix_1
bool exprPostfix(){
	Token *start = iTk;
	if(exprPrimary()){
		if(exprPostfix_1()){
			return true;
		}
	}
	iTk = start;
	return false;
}

//exprUnary: (SUB|NOT) exprUnary | exprPostfix
bool exprUnary(){
	if(consume(SUB) || consume(NOT)){
		if(exprUnary()){
			return true;
		}else tkerr("Lipseste expresia dupa operatorul unar.\n");
	}
	if(exprPostfix()){
		return true;
	}
	return false;
}

//stmCompound: LACC ( varDef | stm )* RACC
bool stmCompound(){
	Token *start = iTk;
	if(consume(LACC)){
		for(;;){
			if(varDef()){}
			else if(stm()){}
			else break;
		}
		if(consume(RACC)){
			return true;
		}else tkerr("Lipseste } dupa corpul functiei\n");
	}
	iTk = start;
	return false;
}

//stm: stmCompound | IF LPAR expr RPAR stm ( ELSE stm )? | WHILE LPAR expr RPAR stm | RETURN expr? SEMICOLON | expr? SEMICOLON
bool stm(){
	if(stmCompound()){
		return true;
	}
	if(consume(IF)){
		if(consume(LPAR)){
			if(expr()){
				if(consume(RPAR)){
					if(stm()){
						if(consume(ELSE)){
							if(stm()){
								return true;
							}else tkerr("Lipseste instructiunea de dupa else\n"); 
						}
						return true;
					}else tkerr("Lipseste instructiunea de dupa )\n");
				}else tkerr("Lipseste ) dupa expresia din if\n");
			}else tkerr("Lipseste expresia din if\n");
		}else tkerr("Lipseste ( dupa if\n");
	}
	if(consume(WHILE)){
		if(consume(LPAR)){
			if(expr()){
				if(consume(RPAR)){
					if(stm()){
						return true;
					}else tkerr("Lipseste instructiunea de dupa )\n");
				}else tkerr("Lipseste ) dupa expresia din while\n");
			}else tkerr("Lipseste expresia din while\n");
		}else tkerr("Lipseste ( dupa while\n");
	}
	if(consume(RETURN)){
		expr();
		if(consume(SEMICOLON)){
			return true;
		}else tkerr("Lipseste ; dupa return\n");
	}
	if(expr()){
		if(consume(SEMICOLON)){
			return true;
		}else tkerr("Lipseste ; dupa expresie\n");
	}
	return false;
}

/*fnDef: ( typeBase | VOID ) ID LPAR ( fnParam ( COMMA fnParam )* )? RPAR stmCompound*/
bool fnDef(){
	Token *start = iTk;
	if(typeBase() || consume(VOID)){
		if(consume(ID)){
			if(consume(LPAR)){
				if(fnParam()){
					while(consume(COMMA)){
						if(!fnParam()){
							tkerr("Lipseste parametrul de dupa virgula.\n");   
						}
					}
				}
				if(consume(RPAR)){
					if(stmCompound()){ 
						return true;
					}else tkerr("Lipseste corpul functiei\n");
				}else tkerr("Lipseste ) dupa lista de parametri\n");
			}else if(!consume(LPAR) && fnParam()) tkerr("Lipseste ( dupa numele functiei\n"); //modificare adusa pentru a se evita intrarea in eroarea "Lipseste ( dupa numele functiei"
		}else tkerr("Lipseste numele functiei\n");
	}
	iTk = start;
	return false;
}

//exprCast: LPAR typeBase arrayDecl? RPAR exprCast | exprUnary
bool exprCast(){
    Token *start = iTk;
	if(consume(LPAR)){
		if(typeBase()){
			arrayDecl();
			if(consume(RPAR)){
				if(exprCast()){
					return true;
				}else tkerr("Lipseste expresia de cast dupa )\n");
			}else tkerr("Lipseste ) inainte de  expresia de cast\n");
		}
        iTk = start;
	}
	if(exprUnary()){
		return true;
	}
	return false;
}

//exprMul: exprMul (MUL|DIV) exprCast | exprCast
//exprMul_1: (MUL|DIV) exprCast exprMul_1 | epsilon
bool exprMul_1(){
	if(consume(MUL)||consume(DIV)){
		if(exprCast()){
			if(exprMul_1()){
				return true;
			}else tkerr("Lipseste expresia de inmultire, urmata de MUL sau DIV si de expresia de cast.\n"); 
		}else tkerr("Lipseste expresia de dupa * sau /\n");
	}
	return true;
}

//exprMul: exprCast exprMul_1
bool exprMul(){
	Token *start = iTk;
	if(exprCast()){
		if(exprMul_1()){
			return true;
		}else tkerr("Lipseste expresia de inmultire, urmata de MUL sau DIV si de expresia de cast \n"); 
	}
	iTk = start;
	return false;
}

//exprAdd: exprAdd (ADD|SUB) exprMul | exprMul
//exprAdd_1: (ADD|SUB) exprMul exprAdd_1 | epsilon
bool exprAdd_1(){
	if(consume(ADD) || consume(SUB)){
		if(exprMul()){
			if(exprAdd_1()){
				return true;
			}else tkerr("Lipseste expresia de adunare.\n");
		}else tkerr("Lipseste expresia de dupa + sau -.\n");
	}
	return true;
}

//exprAdd: exprMul exprAdd_1
bool exprAdd(){
	Token *start = iTk;
	if(exprMul()){
		if(exprAdd_1()){
			return true;
		}else tkerr("Lipseste expresia de adunare.\n");
	}
	iTk = start;
	return false;
}

// unit: ( structDef | fnDef | varDef )* END
bool unit(){
	for(;;){
		if(structDef()){}
		else if(fnDef()){}
		else if(varDef()){}
		else break;
		}
	if(consume(END)){
		return true;
		}
	return false;
}

void parse(Token *tokens){
	iTk=tokens;
	if(!unit())tkerr("syntax error");
}