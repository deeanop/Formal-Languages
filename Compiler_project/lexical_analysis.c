#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include "lexer.h"
#include "utils.h"

Token *tokens;	// single linked list of tokens
Token *lastTk;		// the last token in list

int line=1;		// the current line in the input file
long long int power10(int exponent){
	if(exponent==1)
		return 10;
	else{
		return 10*power10(exponent-1);
	}
}
int sign;
// adds a token to the end of the tokens list and returns it
// sets its code and line
Token *addTk(int code){
	Token *tk=safeAlloc(sizeof(Token));
	tk->code=code;
	tk->line=line;
	tk->next=NULL;
	if(lastTk){
		lastTk->next=tk;
		}else{
		tokens=tk;
		}
	lastTk=tk;
	return tk;
	}

char *extract(const char *begin, const char *end){
	char *rez = safeAlloc(end-begin+1);
	char *current = rez;
	for(int i=0;i<(end-begin);i++){
		*current=begin[i];
		current++;
	}
	*current='\0';
	return rez;
	}

Token *tokenize(const char *pch){
	const char *start;
	Token *tk;
	for(;;){
		switch(*pch){
			case ' ':case '\t':pch++;break;
			case '\r':		// handles different kinds of newlines (Windows: \r\n, Linux: \n, MacOS, OS X: \r or \n)
				if(pch[1]=='\n')pch++;
				// fallthrough to \n
			case '\n':
				line++;
				pch++;
				break;
			case '\0':addTk(END);return tokens;
			case ',':addTk(COMMA);pch++;break;
			case '=':
				if(pch[1]=='='){
					addTk(EQUAL);
					pch+=2;
					}else{
					addTk(ASSIGN);
					pch++;
					}
				break;
			case '#':addTk(HASH);pch++;break;
			case ';':addTk(SEMICOLON);pch++;break;
			case '(':addTk(LPAR);pch++;break;
			case ')':addTk(RPAR);pch++;break;
			case '[':addTk(LBRACKET);pch++;break;
			case ']':addTk(RBRACKET);pch++;break;
			case '{':addTk(LACC);pch++;break;
			case '}':addTk(RACC);pch++;break;
			case '+':addTk(ADD);pch++;break;
			case '-':addTk(SUB);pch++;break;
			case '*':addTk(MUL);pch++;break;
			case '/':
                if(pch[1]=='/'){
                    for(pch+=2;*pch!='\n' && *pch!='\0';pch++){}
                }else{
                addTk(DIV);
                pch++;
                }break;
			case '.':addTk(DOT);pch++;break;
			case '<':
					if(pch[1]=='='){
						addTk(LESSEQ);
						pch+=2;
					}
					else{
						addTk(LESS);pch++;
						pch++;
					}
					break;
			case '>':
					if(pch[1]=='='){
						addTk(GREATEREQ);
						pch+=2;
					}
					else{
						addTk(GREATER);
						pch++;
					}
					break;
			case '&':
					if(pch[1]=='&'){
						addTk(AND);
						pch+=2;
					}else err("invalid char: %c (%d)", *pch, *pch);
					break;
			case '!':
					if(pch[1]=='='){
						addTk(NOTEQ);
						pch+=2;
					}else{
						addTk(NOT);
						pch++;
					}
					break;
			case '|':
					if(pch[1]=='|'){
						addTk(OR);
						pch+=2;
					}else{
						err("invalid char: %c (%d)", *pch, *pch);
					}
					break;
			case '\'':
				pch++;
				for(start=pch;(*pch)!='\'' && (*pch)!='\0';pch++){}
				if(*pch=='\0')
					err("Invalid use of \'.");
				else{
					char *text=extract(start, pch);
					tk=addTk(CHAR);
					tk->c=*text;
					pch++;
				}
				break;
			case '\"':
				pch++;
				for(start=pch;(*pch)!='\"' && (*pch)!='\0';pch++){}
				if(*pch=='\0')
					err("Invalid use of \".");
				else{
					char *text=extract(start, pch);
					tk=addTk(STRING);
					tk->text=text;
					pch++;
				}
				break;
			default:
				if(isalpha(*pch)||*pch=='_'){
					for(start=pch++;isalnum(*pch)||*pch=='_';pch++){}
					char *text=extract(start,pch);
					if(strcmp(text,"char")==0)addTk(TYPE_CHAR);
					else if(strcmp(text, "double")==0)addTk(TYPE_DOUBLE);
					else if(strcmp(text, "int")==0)addTk(TYPE_INT);
					else if(strcmp(text, "else")==0)addTk(ELSE);
					else if(strcmp(text, "if")==0)addTk(IF);
					else if(strcmp(text, "return")==0)addTk(RETURN);
					else if(strcmp(text, "struct")==0)addTk(STRUCT);
					else if(strcmp(text, "void")==0)addTk(VOID);
					else if(strcmp(text, "while")==0)addTk(WHILE);
					else{
						tk=addTk(ID);
						tk->text=text;
						}
				}
				else if(*pch>='0' && *pch<='9'){
					for(start=pch++;*pch>='0' && *pch<='9';pch++){}
					if(*pch=='.' || *pch=='e' || *pch=='E'){
						char *text=extract(start, pch);
						double val=atof(text);
						if(*pch=='.'){
							pch++;
                            if(!(pch[0]>='0' && pch[0]<='9'))
                                err("Missing digits after point at line %d", line);
        
							for(start=pch;*pch>='0' && *pch<='9';pch++){}
							text=extract(start, pch);
							int pow = pch-start;
							val=val*power10(pow);
							val+=atoi(text);
							val/=power10(pow);
						}
						if(*pch=='e' || *pch=='E'){
							pch++;
							int sign=1;
							if(*pch=='+')
                            {
								pch++;
                                if(!(pch[0]>='0' && pch[0]<='9'))
                                    err("Missing digits after exponent at line %d", line);
                            }
							else if(*pch=='-'){
								sign=-1;
								pch++;
                                if(!(pch[0]>='0' && pch[0]<='9'))
                                    err("Missing digits after exponent at line %d", line);
                            }
                            else{
                                if(!(pch[0]>='0' && pch[0]<='9'))
                                    err("Missing exponent at line %d", line);
                            }
							for(start=pch;*pch>='0' && *pch<='9';pch++){}
							text=extract(start, pch);
							int exponent=power10(atoi(text));
							if(sign==1)
								val*=exponent;
							if(sign==-1)
								val/=exponent;
						}
						tk=addTk(DOUBLE);
						tk->d=val;
					}
					else{
						char *text=extract(start, pch);
						tk=addTk(INT);
						tk->i=atoi(text);
					}
				}
				else err("invalid char: %c (%d)",*pch,*pch);
		}
	}
	return tokens;
}

void showTokens(const Token *tokens){
	for(const Token *tk=tokens;tk;tk=tk->next){
		printf("%d",tk->line);
		switch(tk->code){
			case ID:
				printf("ID: %s\n", tk->text);
				break;
			case TYPE_CHAR:
				printf("TYPE_CHAR\n");
				break;
			case TYPE_DOUBLE:
				printf("TYPE_DOUBLE\n");
				break;
			case TYPE_INT:
				printf("TYPE_INT\n");
				break;
			case VOID:
				printf("VOID\n");
				break;
			case STRUCT:
				printf("STRUCT\n");
				break;
			case WHILE:
				printf("WHILE\n");
				break;
			case IF:
				printf("IF\n");
				break;
			case ELSE:
				printf("ELSE\n");
				break;
			case RETURN:
				printf("RETURN\n");
				break;
			case END:
				printf("END\n");
				break;
			case COMMA:
				printf("COMMA\n");
				break;
			case ASSIGN:
				printf("ASSIGN\n");
				break;
			case SEMICOLON:
				printf("SEMICOLON\n");
				break;
			case LPAR:
				printf("LPAR\n");
				break;
			case RPAR:
				printf("RPAR\n");
				break;
			case LBRACKET:
				printf("LBRACKET\n");
				break;
			case RBRACKET:
				printf("RBRACKET\n");
				break;
			case LACC:
				printf("LACC\n");
				break;
			case RACC:
				printf("RACC\n");
				break;
			case ADD:
				printf("ADD\n");
				break;
			case SUB:
				printf("SUB\n");
				break;
			case MUL:
				printf("MUL\n");
				break;
			case DIV:
				printf("DIV\n");
				break;
			case DOT:
				printf("DPT\n");
				break;
			case AND:
				printf("AND\n");
				break;
			case OR:
				printf("OR\n");
				break;
			case NOT:
				printf("NOT\n");
				break;
			case EQUAL:
				printf("EQUAL\n");
				break;
			case NOTEQ:
				printf("NOTEQ\n");
				break;
			case LESS:
				printf("LESS\n");
				break;
			case LESSEQ:
				printf("LESSEQ\n");
				break;
			case GREATER:
				printf("GREATER\n");
				break;
			case GREATEREQ:
				printf("GREATEREQ\n");
				break;
			case INT:
				printf("INT: %d\n", tk->i);
				break;
			case DOUBLE:
				printf("DOUBLE: %.2f\n", tk->d);
				break;
			case STRING:
				printf("STRING: %s\n", tk->text);
				break;
			case CHAR:
				printf("CHAR: %c\n", tk->c);
				break;
			case HASH:
				printf("HASH\n");
				break;
			default:
				printf("Unknown token.\n");
		}
	}
}
