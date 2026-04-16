#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include "parser.h"
#include "ad.h"
#include "at.h"
#include "gc.h"
#include "lexer.h"
#include "utils.h"

Token *iTk;		// the iterator in the tokens list
Token *consumedTk;		// the last consumed token
Symbol *owner = NULL;

bool varDef();
bool typeBase(Type *t);
bool arrayDecl(Type *t);
bool fnDef();
bool fnParam();
bool stm();
bool stmCompound(bool newDomain);
bool expr(Ret *r);
bool exprRel(Ret *r);
bool exprEq(Ret *r);
bool exprAdd(Ret *r);
bool exprMul(Ret *r);
bool exprCast(Ret *r);
bool exprUnary(Ret *r);
bool exprPostfix(Ret *r);
bool exprPrimary(Ret *r);
bool exprAnd(Ret *r);
bool exprOr(Ret *r);


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

// typeBase[out Type *t]: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
bool typeBase(Type *t){
	t->n=-1;
	if(consume(TYPE_INT)){
		t->tb = TB_INT;
		return true;
	}
	if(consume(TYPE_DOUBLE)){
		t->tb = TB_DOUBLE;
		return true;
	}
	if(consume(TYPE_CHAR)){
		t->tb = TB_CHAR;
		return true;
	}
	if(consume(STRUCT)){
		if(consume(ID)){
			Token *tkName = consumedTk;
			t->tb = TB_STRUCT;
			t->s = findSymbol(tkName->text);
			if(!t->s) tkerr("Structura nedefinita: %s", tkName->text);
			return true;
		}tkerr("Lipseste numele structurii\n");
	}
	return false;
}
	
//arrayDecl[inout Type *t]: LBRACKET INT? RBRACKET
bool arrayDecl(Type *t){
	Token *start = iTk;
	if(consume(LBRACKET)){
		if(consume(INT)){
			Token *tkSize = consumedTk;
			t->n=tkSize->i;
		}else{
			t->n=0;
		}
		if(consume(RBRACKET)){
			return true;
		}else tkerr("Lipseste ] dupa dimensiunea array-ului\n");
	}
	iTk = start;
	return false;
}

//varDef: typeBase[&t] ID[tkName] arrayDecl[&t]? SEMICOLON
bool varDef(){
	Type t;
	Token *start = iTk;
	if(typeBase(&t)){
		if(consume(ID)){
			Token *tkName = consumedTk;
			arrayDecl(&t);
			if(t.n==0) tkerr("varDef:O variabila array trebuie sa aiba o dimensiune specificata.\n");
			if(consume(SEMICOLON)){
				Symbol *var = findSymbolInDomain(symTable, tkName->text);
				if(var) tkerr("varDef: Redefinire de simbol: %s", tkName->text);  //adaugare indice metoda la mesajul de eroare
				var = newSymbol(tkName->text, SK_VAR);
				var->type = t;
				var->owner = owner;
				addSymbolToDomain(symTable, var);
				if(owner){
					switch(owner->kind){
						case SK_FN:
							var->varIdx = symbolsLen(owner->fn.locals);
							addSymbolToList(&owner->fn.locals, dupSymbol(var));
							break;
						case SK_STRUCT:
							var->varIdx = typeSize(&owner->type);
							addSymbolToList(&owner->structMembers, dupSymbol(var));
							break;
					}
				}else{
					var->varMem = safeAlloc(typeSize(&t));
				}
				return true;
			}else tkerr("Lipseste ; dupa declaratia de variabila\n");
		}else tkerr("Lipseste numele variabilei\n");
	}
	iTk=start;
	return false;
}

//structDef: STRUCT ID[tkName] LACC varDef* RACC SEMICOLON
bool structDef(){
	Token *start = iTk;
	if(consume(STRUCT)){
		if(consume(ID)){
			Token *tkName = consumedTk;
			if(consume(LACC)){
				Symbol *s = findSymbolInDomain(symTable, tkName->text);
				if(s) tkerr("structDef: Redefinire de simbol: %s", tkName->text); //adaugare indice metoda la mesajul de eroare
				s = addSymbolToDomain(symTable, newSymbol(tkName->text, SK_STRUCT));
				s->type.tb = TB_STRUCT;
				s->type.s = s;
				s->type.n = -1;
				pushDomain();
				owner = s;
				while(varDef()){}
				if(consume(RACC)){
					if(consume(SEMICOLON)){
						owner = NULL;
						dropDomain();
						return true;
					}else tkerr("Lipseste ; dupa struct\n");
				}else tkerr("Lipseste } dupa struct\n");
			}else if(!consume(LACC) && varDef()) tkerr("Lipseste { dupa struct\n"); //modificare adusa pentru a se evita intrarea in eroarea "Lipseste { dupa struct"
		}else tkerr("Lipseste numele structurii\n");
	}
	iTk = start;
	return false;
}

//fnParam: typeBase[&t] ID[tkName] arrayDecl[&t]?
bool fnParam(){
	Type t;
	Token *start = iTk;
	if(typeBase(&t)){
		if(consume(ID)){
			Token *tkName = consumedTk;
			if(arrayDecl(&t))
				t.n=0; // pentru a evita eroarea "O variabila array trebuie sa aiba o dimensiune specificata"
			Symbol *param = findSymbolInDomain(symTable, tkName->text);
			if(param) tkerr("fnParam: Redefinire de simbol: %s", tkName->text); //adaugare indice metoda la mesajul de eroare
			param = newSymbol(tkName->text, SK_PARAM);
			param->type = t;
			param->owner = owner;
			param->paramIdx = symbolsLen(owner->fn.params);
			addSymbolToDomain(symTable, param);
			addSymbolToList(&owner->fn.params, dupSymbol(param));
			return true;
		}else tkerr("Lipseste numele parametrului\n");
	}
	iTk = start;
	return false;
}

//exprRel: exprRel(LESS | LESSEQ | GREATER | GREATEREQ) exprAdd | exprAdd
//exprRel_1: (LESS | LESSEQ | GREATER | GREATEREQ) exprAdd exprRel_1 | epsilon
bool exprRel_1(Ret *r){
	if(consume(LESS)){
		Ret right;
		Token *op;
		op = iTk - 1;
		Instr *lastLeft = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
        if(exprAdd(&right)){
            Type tDst;
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch(tDst.tb){
				case TB_INT: addInstr(&owner->fn.instr, OP_LESS_I); break;
				case TB_DOUBLE: addInstr(&owner->fn.instr, OP_LESS_F); break;
			}
				
            if(!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("Operand invalid pentru <.\n");
            *r = (Ret){{TB_INT, NULL, -1}, false, true};
			if(exprRel_1(r)){
				return true;
			}else tkerr("Lipseste expresia relationala.\n");
		}
	}
    if(consume(LESSEQ)){
        Ret right;
		Token *op;
		op = iTk - 1;
		Instr *lastLeft = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
        if(exprAdd(&right)){
            Type tDst;
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch(tDst.tb){
				case TB_INT: addInstr(&owner->fn.instr, OP_LESS_I); break;
				case TB_DOUBLE: addInstr(&owner->fn.instr, OP_LESS_F); break;
			}
            if(!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("Operand invalid pentru <=.\n");
            *r = (Ret){{TB_INT, NULL, -1}, false, true};
			if(exprRel_1(r)){
				return true;
			}else tkerr("Lipseste expresia relationala.\n");
		}
    }
    if(consume(GREATER)){
        Ret right;
		Token *op;
		op = iTk - 1;
		Instr *lastLeft = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
        if(exprAdd(&right)){
            Type tDst;
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch(tDst.tb){
				case TB_INT: addInstr(&owner->fn.instr, OP_LESS_I); break;
				case TB_DOUBLE: addInstr(&owner->fn.instr, OP_LESS_F); break;
			}
            if(!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("Operand invalid pentru >.\n");
            *r = (Ret){{TB_INT, NULL, -1}, false, true};
			if(exprRel_1(r)){
				return true;
			}else tkerr("Lipseste expresia relationala.\n");
		}
    }
    if(consume(GREATEREQ)){
        Ret right;
		Token *op;
		op = iTk - 1;
		Instr *lastLeft = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
        if(exprAdd(&right)){
            Type tDst;
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch(tDst.tb){
				case TB_INT: addInstr(&owner->fn.instr, OP_LESS_I); break;
				case TB_DOUBLE: addInstr(&owner->fn.instr, OP_LESS_F); break;
			}
            if(!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("Operand invalid pentru >=.\n");
            *r = (Ret){{TB_INT, NULL, -1}, false, true};
			if(exprRel_1(r)){
				return true;
			}else tkerr("Lipseste expresia relationala.\n");
		}
    }
	return true;  
}

//exprRel: exprAdd exprRel_1
bool exprRel(Ret *r){
	Token *start = iTk;
	if(exprAdd(r)){
		if(exprRel_1(r)){
			return true;
		}else tkerr("Lipseste expresia relationala.\n");
	}
	iTk = start;
	return false;
}

//exprEq: exprEq (EQUAL | NOTEQ) exprRel | exprRel
//exprEq_1: (EQUAL| NOTEQ) exprRel exprEq_1 | epsilon
bool exprEq_1(Ret *r){
	if(consume(EQUAL)){
        Ret right;
		if(exprRel(&right)){
            Type tDst;
            if(!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("Operand invalid pentru ==.\n");
            *r = (Ret){{TB_INT, NULL, -1}, false, true};
			if(exprEq_1(r)){
				return true;
			}else tkerr("Lipseste expresia relationala, precedata de ==.\n");
		}else tkerr("Lipseste expresia de dupa == sau !=.\n");
	}
    if(consume(NOTEQ)){
        Ret right;
		if(exprRel(&right)){
            Type tDst;
            if(!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("Operand invalid pentru !=.\n");
            *r = (Ret){{TB_INT, NULL, -1}, false, true};
			if(exprEq_1(r)){
				return true;
			}else tkerr("Lipseste expresia relationala, precedata de !=.\n");
		}else tkerr("Lipseste expresia de dupa == sau !=.\n");
    }
    return true;
}

//exprEq: exprRel exprEq_1
bool exprEq(Ret *r){
	Token *start = iTk;
	if(exprRel(r)){
		if(exprEq_1(r)){
			return true;
		}else tkerr("Lipseste expresia relationala, precedata de == sau !=.\n"); 
	}
	iTk = start;
	return false;
}

//exprAnd: exprAnd AND exprEq | exprEq
//exprAnd_1: AND exprEq exprAnd_1| epsilon
bool exprAnd_1(Ret *r){
	if(consume(AND)){
        Ret right;
		if(exprEq(&right)){
            Type tDst;
            if(!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("Operand invalid pentru &&.\n");
            *r = (Ret){{TB_INT, NULL, -1}, false, true};
			if(exprAnd_1(r)){ 
				return true;
			}else tkerr("Lipseste expresia de dupa &&.\n");
		}else tkerr("Lipseste expresia de dupa &&.\n");
	}
	return true;
}

//exprAnd: exprEq exprAnd_1
bool exprAnd(Ret *r){
	Token *start = iTk;
	if(exprEq(r)){
		if(exprAnd_1(r)){
			return true;
		}else tkerr("Lipseste expresia de dupa &&.\n");
	}
	iTk = start;
	return false;
}

//exprOr: exprOr OR exprAnd | exprAnd
//exprOr_1: OR exprAnd exprOr_1 | epsilon
bool exprOr_1(Ret *r){
	if(consume(OR)){
        Ret right;
		if(exprAnd(&right)){
            Type tDst;
            if(!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("Operand invalid pentru ||.\n");
            *r = (Ret){{TB_INT, NULL, -1}, false, true};
			if(exprOr_1(r)){
				return true;
			}else tkerr("Lipseste expresia de dupa ||.\n");
		}else tkerr("Lipseste expresia de dupa ||.\n");
	}
	return true;
}

//exprOr: exprAnd exprOr_1
bool exprOr(Ret *r){
	Token *start = iTk;
	if(exprAnd(r)){
		if(exprOr_1(r)){
			return true;
		}else tkerr("Lipseste expresia de dupa ||.\n");
	}
	iTk = start;
	return false;
}

//exprAssign: exprUnary ASSIGN exprAssign | exprOr
bool exprAssign(Ret *r) {
    Ret rDst;
    Token *start = iTk;

    if (exprUnary(&rDst)) {
        if (consume(ASSIGN)) {
            if (exprAssign(r)) {

                if (!rDst.lval) tkerr("Destinația asignării trebuie să fie o valoare de stânga.\n");
                if (rDst.ct) tkerr("Destinația asignării nu poate fi constantă.\n");
                if (!canBeScalar(&rDst)) tkerr("Destinația asignării trebuie să fie scalara.\n");
                if (!canBeScalar(r)) tkerr("Sursa asignării trebuie să fie scalara.\n");
                if (!convTo(&r->type, &rDst.type)) tkerr("Tipul sursei nu poate fi convertit la tipul destinației.\n");

                addRVal(&owner->fn.instr, r->lval, &r->type);
                insertConvIfNeeded(lastInstr(owner->fn.instr), &r->type, &rDst.type);

                switch (rDst.type.tb) {
                    case TB_INT:    addInstr(&owner->fn.instr, OP_STORE_I); break;
                    case TB_DOUBLE: addInstr(&owner->fn.instr, OP_STORE_F); break;
                    default:        tkerr("Tip nesuportat la asignare.\n");
                }

                r->lval = false; 
                r->ct = true;    
                r->type = rDst.type; 

                return true;
            } else {
                tkerr("Lipsește expresia din dreapta semnului =.\n");
            }
        }
    }

    iTk = start;
    if (exprOr(r)) return true;

    return false;
}


//expr: exprAssign
bool expr(Ret *r){
	if(exprAssign(r)){
		return true;
	}
	return false;
}

//exprPrimary: ID ( LPAR ( expr ( COMMA expr )* )? RPAR )? | INT | DOUBLE | CHAR | STRING | LPAR expr RPAR
bool exprPrimary(Ret *r){
    Type t;
    if(consume(ID)){
        Token *tkName = consumedTk;
        Symbol *s = findSymbol(tkName->text);
        if(!s) tkerr("Identificator nedefinit: %s\n", tkName->text);

        if(consume(LPAR)){
            if(s->kind != SK_FN)
                tkerr("Doar o functie poate fi apelata.\n");

            Ret rArg;
            Symbol *param = s->fn.params;

            if(expr(&rArg)){
                addRVal(&owner->fn.instr, rArg.lval, &rArg.type);
                insertConvIfNeeded(lastInstr(owner->fn.instr), &rArg.type, &param->type);

                if(!param)
                    tkerr("Prea multe argumente in apelul functiei.\n");

                if(!convTo(&rArg.type, &param->type))
                    tkerr("Nu se poate converti in apel tipul argumentului la tipul parametrului.\n");

                param = param->next;

                while(consume(COMMA)){
                    if(!expr(&rArg))
                        tkerr("Lipseste expresia de dupa virgula.\n");

                    addRVal(&owner->fn.instr, rArg.lval, &rArg.type);
                    insertConvIfNeeded(lastInstr(owner->fn.instr), &rArg.type, &param->type);

                    if(!param)
                        tkerr("Prea multe argumente in apelul functiei.\n");

                    if(!convTo(&rArg.type, &param->type))
                        tkerr("Nu se poate converti in apel tipul argumentului la tipul parametrului.\n");

                    param = param->next;
                }
            }

            if(consume(RPAR)){
                if(s->fn.extFnPtr){
                    addInstr(&owner->fn.instr, OP_CALL_EXT)->arg.extFnPtr = s->fn.extFnPtr;
                } else {
                    addInstr(&owner->fn.instr, OP_CALL)->arg.instr = s->fn.instr;
                }
            } else {
                tkerr("Lipseste ) dupa argumentele functiei.\n");
            }

            if(param)
                tkerr("Prea putine argumente in apelul functiei.\n");

            *r = (Ret){s->type, false, true};
            return true;
        }

        if(s->kind == SK_FN){
            tkerr("O functie trebuie apelata, nu folosita ca variabila.\n");
        }

        // variabila sau parametru
        *r = (Ret){s->type, true, s->type.n >= 0};
        return true;
    }

    if(consume(INT)){
        Token *ct = consumedTk;
        addInstrWithInt(&owner->fn.instr, OP_PUSH_I, ct->i);
        *r = (Ret){{TB_INT, NULL, -1}, false, true};
        return true;
    }

    if(consume(DOUBLE)){
        Token *ct = consumedTk;
        addInstrWithDouble(&owner->fn.instr, OP_PUSH_F, ct->d);
        *r = (Ret){{TB_DOUBLE, NULL, -1}, false, true};
        return true;
    }

    if(consume(CHAR)){
        Token *ct = consumedTk;
        addInstrWithInt(&owner->fn.instr, OP_PUSH_C, ct->c); // dacă există opcode pentru char
        *r = (Ret){{TB_CHAR, NULL, -1}, false, true};
        return true;
    }

    if(consume(STRING)){
        Token *ct = consumedTk;
        addInstrWithString(&owner->fn.instr, OP_PUSH_STR, ct->text); // presupun că există așa ceva
        *r = (Ret){{TB_CHAR, NULL, 0}, false, true};
        return true;
    }

    if(consume(LPAR)){
        if(expr(r)){
            if(consume(RPAR)){
                return true;
            } else {
                tkerr("Lipseste ) dupa expresia din paranteza\n");
            }
        }
    }

    return false;
}

//exprPostfix: exprPostfix LBRACKET expr RBRACKET | exprPostfix DOT ID | exprPrimary
//exprPostfix_1: LBRACKET expr RBRACKET exprPostfix_1 | DOT ID exprPostfix_1 | epsilon
bool exprPostfix_1(Ret *r){
	if(consume(LBRACKET)){
        Ret idx;
		Type t;
		if(expr(&idx)){
			if(consume(RBRACKET)){
                if(r->type.n<0) tkerr("Doar un array poate fi indexat.\n");
                Type tInt = {TB_INT, NULL, -1};
                if(!convTo(&idx.type, &tInt)) tkerr("Indexul nu poate fi convertit la int.\n");
                r->type.n=-1;
                r->lval = true;
                r->ct = false;
				if(exprPostfix_1(r)){
					return true;
				}
			}else tkerr("Lipseste ] dupa expresia din paranteza.\n");
		}else tkerr("Lipseste expresia din paranteza.\n");
	}
	if(consume(DOT)){
		if(consume(ID)){
            Token *tkName = consumedTk;
            if(r->type.tb!=TB_STRUCT) tkerr("Un camp poate fi selectat doar dintr-un struct.\n");
            Symbol *s = findSymbolInList(r->type.s->structMembers, tkName->text);
            if(!s) tkerr("Structura %s nu are un camp %s.\n", r->type.s->name, tkName->text);
            *r=(Ret){s->type,true,s->type.n>=0};
            if(exprPostfix_1(r)){
				return true;
			}
		}else tkerr("Lipseste identificatorul.\n");
	}
	return true;
}

//exprPostfix: exprPrimary exprPostfix_1
bool exprPostfix(Ret *r){
	Token *start = iTk;
	if(exprPrimary(r)){
		if(exprPostfix_1(r)){
			return true;
		}
	}
	iTk = start;
	return false;
}

//exprUnary: (SUB|NOT) exprUnary | exprPostfix
bool exprUnary(Ret *r){
	if(consume(SUB)){
		if(exprUnary(r)){
            if(!canBeScalar(r)) tkerr("- unar trebuie sa aiba un operand scalar.\n");
            r->lval = false;
            r->ct = true;
			return true;
		}else tkerr("Lipseste expresia dupa operatorul unar.\n");
	}
    if(consume(NOT)){
		if(exprUnary(r)){
            if(!canBeScalar(r)) tkerr("! unar trebuie sa aiba un operand scalar.\n");
            r->lval = false;
            r->ct = true;
			return true;
		}else tkerr("Lipseste expresia dupa operatorul unar.\n");
	}
	if(exprPostfix(r)){
		return true;
	}
	return false;
}

//stmCompound[in bool newDomain]: LACC ( varDef | stm )* RACC
bool stmCompound(bool newDomain){
	Token *start = iTk;
    Ret rCond, rExpr;
	if(consume(LACC)){
		if(newDomain)
			pushDomain();
		for(;;){
			if(varDef()){}
			else if(stm()){}
			else break;
		}
		if(consume(RACC)){
			if(newDomain){
				dropDomain();
			}
			return true;
		}else tkerr("Lipseste } dupa corpul functiei\n");
	}
	iTk = start;
	return false;
}

//stm: stmCompound[true] | IF LPAR expr RPAR stm ( ELSE stm )? | WHILE LPAR expr RPAR stm | RETURN expr? SEMICOLON | expr? SEMICOLON
bool stm(){
	Ret rCond, rExpr;

	if(stmCompound(true)){
		return true;
	}

	if(consume(IF)){
		if(consume(LPAR)){
			if(expr(&rCond)){
				if(!canBeScalar(&rCond)) tkerr("Condiția din if trebuie să fie o valoare scalara.\n");
				if(consume(RPAR)){
					addRVal(&owner->fn.instr, rCond.lval, &rCond.type);
					Type intType = {TB_INT, NULL, -1};
					insertConvIfNeeded(lastInstr(owner->fn.instr), &rCond.type, &intType);
					Instr *ifJF = addInstr(&owner->fn.instr, OP_JF);

					if(stm()){
						if(consume(ELSE)){
							Instr *ifJMP = addInstr(&owner->fn.instr, OP_JMP);
							ifJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
							if(stm()){
								ifJMP->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
								return true;
							} else {
								tkerr("Lipsește instrucțiunea de după else\n");
							}
						} else {
							ifJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
						}
						return true;
					} else {
						tkerr("Lipsește instrucțiunea de după ) în if\n");
					}
				} else tkerr("Lipsește ) după expresia din if\n");
			} else tkerr("Lipsește expresia din if\n");
		} else tkerr("Lipsește ( după if\n");
	}

	if(consume(WHILE)){
		Instr *beforeWhileCond = lastInstr(owner->fn.instr);
		if(consume(LPAR)){
			if(expr(&rCond)){
				if(!canBeScalar(&rCond)) tkerr("Condiția din while trebuie să fie o valoare scalara.\n");
				if(consume(RPAR)){
					addRVal(&owner->fn.instr, rCond.lval, &rCond.type);
					Type intType = {TB_INT, NULL, -1};
					insertConvIfNeeded(lastInstr(owner->fn.instr), &rCond.type, &intType);
					Instr *whileJF = addInstr(&owner->fn.instr, OP_JF);
					if(stm()){
						addInstr(&owner->fn.instr, OP_JMP)->arg.instr = beforeWhileCond->next;
						whileJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
						return true;
					} else {
						tkerr("Lipsește instrucțiunea de după ) în while\n");
					}
				} else tkerr("Lipsește ) după expresia din while\n");
			} else tkerr("Lipsește expresia din while\n");
		} else tkerr("Lipsește ( după while\n");
	}

	if(consume(RETURN)){
		if(expr(&rExpr)){
			if(owner->type.tb == TB_VOID)
				tkerr("O funcție void nu poate returna o valoare.\n");
			if(!canBeScalar(&rExpr))
				tkerr("Valoarea returnată trebuie să fie scalara.\n");
			if(!convTo(&rExpr.type, &owner->type))
				tkerr("Tipul returnat nu este compatibil cu tipul funcției.\n");

			addRVal(&owner->fn.instr, rExpr.lval, &rExpr.type);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &rExpr.type, &owner->type);
			addInstrWithInt(&owner->fn.instr, OP_RET, symbolsLen(owner->fn.params));
		} else {
			if(owner->type.tb != TB_VOID)
				tkerr("O funcție non-void trebuie să returneze o valoare.\n");
			addInstr(&owner->fn.instr, OP_RET_VOID);
		}

		if(consume(SEMICOLON)){
			return true;
		} else tkerr("Lipsește ; după return\n");
	}

	if(expr(&rExpr)){
		if(rExpr.type.tb != TB_VOID)
			addInstr(&owner->fn.instr, OP_DROP);
		if(consume(SEMICOLON)){
			return true;
		} else tkerr("Lipsește ; după expresie\n");
	}

	return false;
}


/*fnDef: ( typeBase[&t] | VOID ) ID[tkName] LPAR ( fnParam ( COMMA fnParam )* )? RPAR stmCompound[false]*/
bool fnDef(){
	Type t;
	Token *start = iTk;
	if(typeBase(&t)){
		if(consume(ID)){
			Token *tkName = consumedTk;
			if(consume(LPAR)){
				Symbol *fn = findSymbolInDomain(symTable, tkName->text);
				if(fn) tkerr("fnDef: Redefinire de simbol: %s", tkName->text); //adaugare indice metoda la mesajul de eroare
				fn = newSymbol(tkName->text, SK_FN);
				fn->type=t;
				addSymbolToDomain(symTable, fn);
				owner=fn;
				pushDomain();
				if(fnParam()){
					while(consume(COMMA)){
						if(!fnParam()){
							tkerr("Lipseste parametrul de dupa virgula.\n");   
						}
					}
				}
				if(consume(RPAR)){
					addInstr(&fn->fn.instr,OP_ENTER);
					if(stmCompound(false)){
						fn->fn.instr->arg.i=symbolsLen(fn->fn.locals);
						if(fn->type.tb==TB_VOID)
							addInstrWithInt(&fn->fn.instr,OP_RET_VOID,symbolsLen(fn->fn.params));
						dropDomain();
						owner = NULL; 
						return true;
					}else tkerr("Lipseste corpul functiei\n");
				}else tkerr("Lipseste ) dupa lista de parametri\n");
			}else if(!consume(LPAR) && fnParam()) tkerr("Lipseste ( dupa numele functiei\n"); //modificare adusa pentru a se evita intrarea in eroarea "Lipseste ( dupa numele functiei"
		}else tkerr("Lipseste numele functiei\n");
	}
	if(consume(VOID)){
		t.tb = TB_VOID; // adaugare pentru a permite definirea functiilor de tip void
		if(consume(ID)){
			Token *tkName = consumedTk;
			if(consume(LPAR)){
				Symbol *fn = findSymbolInDomain(symTable, tkName->text);
				if(fn) tkerr("fnDef: Redefinire de simbol: %s", tkName->text);
				fn = newSymbol(tkName->text, SK_FN);
				fn->type=t;
				addSymbolToDomain(symTable, fn);
				owner=fn;
				pushDomain();
				if(fnParam()){
					while(consume(COMMA)){
						if(!fnParam()){
							tkerr("Lipseste parametrul de dupa virgula.\n");   
						}
					}
				}
				if(consume(RPAR)){
					addInstr(&fn->fn.instr,OP_ENTER);
					if(stmCompound(false)){ 
						fn->fn.instr->arg.i=symbolsLen(fn->fn.locals);
						if(fn->type.tb==TB_VOID)
							addInstrWithInt(&fn->fn.instr,OP_RET_VOID,symbolsLen(fn->fn.params));
						dropDomain();
						owner = NULL; 
						return true;
					}else tkerr("Lipseste corpul functiei\n");
				}else tkerr("Lipseste ) dupa lista de parametri\n");
			}else if(!consume(LPAR) && fnParam()) tkerr("Lipseste ( dupa numele functiei\n"); //modificare adusa pentru a se evita intrarea in eroarea "Lipseste ( dupa numele functiei"
		}else tkerr("Lipseste numele functiei\n");
	}
	iTk = start;
	return false;
}

//exprCast: LPAR typeBase[&t] arrayDecl[&t]? RPAR exprCast | exprUnary
bool exprCast(Ret *r){
    Token *start = iTk;
	if(consume(LPAR)){
		Type t;
        Ret op;
		if(typeBase(&t)){
			arrayDecl(&t);
			if(consume(RPAR)){
				if(exprCast(&op)){
                    if(t.tb == TB_STRUCT) tkerr("Nu se poate converti la tipul struct.\n");
                    if(op.type.tb == TB_STRUCT) tkerr("Nu se poate converti o structura.\n");
                    if(op.type.n>=0 && t.n<0) tkerr("Un array poate fi convertit doar la un alt array.\n");
                    if(op.type.n<0 && t.n>=0) tkerr("Un scalar poate fi convertit doar la un alt scalar.\n");
                    *r = (Ret){t, false, true};
					return true;
				}else tkerr("Lipseste expresia de cast dupa )\n");
			}else tkerr("Lipseste ) inainte de  expresia de cast\n");
		}
        iTk = start;
		return false;
	}
	if(exprUnary(r)){
		return true;
	}
	return false;
}

//exprMul: exprMul (MUL|DIV) exprCast | exprCast
//exprMul_1: (MUL|DIV) exprCast exprMul_1 | epsilon
bool exprMul_1(Ret *r){
	if(consume(MUL)){
        Ret right;
		Token *op;
		*op = iTk - 1;
		Instr *lastLeft = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
		if(exprCast(&right)){
            Type tDst;
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch(tDst.tb){
				case TB_INT: addInstr(&owner->fn.instr, OP_MUL_I); break;
				case TB_DOUBLE: addInstr(&owner->fn.instr, OP_MUL_F); break;
			}
            if(!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("Operand invalid pentru *.\n");
            *r = (Ret){tDst, false, true};
			if(exprMul_1(r)){
				return true;
			}else tkerr("Lipseste expresia de inmultire, urmata de MUL sau DIV si de expresia de cast.\n"); 
		}else tkerr("Lipseste expresia de dupa *.\n");
	}
    if(consume(DIV)){
        Ret right;
		Token *op;
		*op = iTk - 1;
		Instr *lastLeft = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
		if(exprCast(&right)){
            Type tDst;
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch(tDst.tb){
				case TB_INT: addInstr(&owner->fn.instr, OP_DIV_I); break;
				case TB_DOUBLE: addInstr(&owner->fn.instr, OP_DIV_F); break;
			}
            if(!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("Operand invalid pentru /.\n");
            *r = (Ret){tDst, false, true};
			if(exprMul_1(r)){
				return true;
			}else tkerr("Lipseste expresia de inmultire, urmata de MUL sau DIV si de expresia de cast.\n"); 
		}else tkerr("Lipseste expresia de dupa /.\n");
	}
	return true;
}

//exprMul: exprCast exprMul_1
bool exprMul(Ret *r){
	Token *start = iTk;
	if(exprCast(r)){
		if(exprMul_1(r)){
			return true;
		}else tkerr("Lipseste expresia de inmultire, urmata de MUL sau DIV si de expresia de cast \n"); 
	}
	iTk = start;
	return false;
}

//exprAdd: exprAdd (ADD|SUB) exprMul | exprMul
//exprAdd_1: (ADD|SUB) exprMul exprAdd_1 | epsilon
bool exprAdd_1(Ret *r){
	if(consume(ADD)){
		Ret right;
		Token *op;
		*op = iTk - 1;
		Instr *lastLeft = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
		if(exprMul(&right)){
            Type tDst;
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch(tDst.tb){
				case TB_INT: addInstr(&owner->fn.instr, OP_ADD_I); break;
				case TB_DOUBLE: addInstr(&owner->fn.instr, OP_ADD_F); break;
			}
            if(!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("Operand invalid pentru +.\n");
            *r = (Ret){tDst, false, true};
			if(exprAdd_1(r)){
				return true;
			}else tkerr("Lipseste expresia de adunare.\n");
		}else tkerr("Lipseste expresia de dupa +.\n");
	}
    if(consume(SUB)){
        Ret right;
		Token *op;
		*op = iTk - 1;
		Instr *lastLeft = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
		if(exprMul(&right)){
            Type tDst;
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch(tDst.tb){
				case TB_INT: addInstr(&owner->fn.instr, OP_SUB_I); break;
				case TB_DOUBLE: addInstr(&owner->fn.instr, OP_SUB_F); break;
			}
            if(!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("Operand invalid pentru -.\n");
            *r = (Ret){tDst, false, true};
			if(exprAdd_1(r)){
				return true;
			}else tkerr("Lipseste expresia de adunare.\n");
		}else tkerr("Lipseste expresia de dupa -.\n");
	}
	return true;
}

//exprAdd: exprMul exprAdd_1
bool exprAdd(Ret *r){
	Token *start = iTk;
	if(exprMul(r)){
		if(exprAdd_1(r)){
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
