/****************************************************/
/* 					            */
/* Analisador Sintatico                             */
/* Daiana Santos e Isadora Muniz                    */
/****************************************************/

%{
	
	#define YYPARSER    
    #include "globals.h"
    #include "util.h"
    #include "scan.h"
    #include "parse.h"

    #define YYSTYPE TreeNode *
    int yyerror(char* message);
    static TreeNode * savedTree;   
    static int yylex(void);

%}

%token IF ELSE WHILE INT VOID RETURN
%token NUM ID
%token IGL IGLIGL DIF MEN MAI MEIGL MAIGL 
%token SOM SUB
%token MUL DIV
%token APR FPR ACO FCO VIR ACH FCH PEV 
%token ERR ENDFILE 
%expect 1 

%%

programa: 			declaracaoLista 
 					{ savedTree = $1; }
                    
	;

declaracaoLista:		declaracaoLista declaracao 
						{ YYSTYPE t = $1;
                            if(t != NULL)
		   	  			    {
                                while(t->sibling != NULL)
                                    t = t->sibling;
                                t->sibling = $2;
                                $$ = $1;
                            }
                            else
                                $$ = $2;
                        }
	|			declaracao
				{ $$ = $1; }
	;
declaracao:		varDeclaracao 
				{$$ = $1;}
	|			funDeclaracao
				{$$ = $1;}
	;
varDeclaracao:	TipoEspecificador identificador	PEV
                {	
                        	$$ = newExpNode(typeK);
                            $$->type = $1->type;
                            $$->child[0] = $2;
                            $2->nodekind = statementK;
                            $2->kind.stmt = variableK;
							$2->type = $1->type;
                }

	|			TipoEspecificador identificador ACO numero FCO PEV
                {
                        	$$ = newExpNode(typeK);
                            $$->type = $1->type;
                            $$->child[0] = $2;
                            $2->nodekind = statementK;
                            $2->kind.stmt = variableK;
							$2->type = $1->type; 
                            $2->attr.len = $4->attr.val;
                }
	;

TipoEspecificador:	INT
                {
						  $$ = newExpNode(typeK);
						  $$->type = integerK;
           				  $$->attr.name = "integer";
				}
	|			VOID
                {
						  $$ = newExpNode(typeK);
						  $$->type = voidK;
           				  $$->attr.name = "void";
				}
	;	

funDeclaracao:		TipoEspecificador identificador APR params FPR compostodecl
                {
                        	$$ = newExpNode(typeK);
                            $$->type = $1->type;
                            $$->child[0] = $2;
                            $2->child[0] = $4;
                            $2->child[1] = $6;
                            $2->nodekind = statementK;
                            $2->kind.stmt = functionK;
							$2->type = $1->type;
							$4->type = $1->type;
							aggScope($2->child[0], $2->attr.name);
							aggScope($2->child[1], $2->attr.name);
                 }
	;
params:			paramlista
				{ $$ = $1; }
	|			TipoEspecificador 
                {
						  $$ = newExpNode(typeK);
				}
	;
paramlista:		paramlista VIR param
				{
                           YYSTYPE t = $1;
                           if(t != NULL)
						   {
                              while(t->sibling != NULL)
                                  t = t->sibling;
                              t->sibling = $3;
                              $$ = $1;
                            }
                            else
                              $$ = $3;
                 }
	|			param
				{ $$ = $1; }
	;
param:			TipoEspecificador identificador
                {
						   	
                           $$ = newExpNode(typeK);
					       $2->nodekind = statementK;
                           $2->kind.stmt = variableK;
                           $$->type = $1->type;
						   $2->type = $1->type;
                           $$->child[0] = $2;
                 } 
	|			TipoEspecificador identificador ACO FCO
                {
						   	
                           $$ = newExpNode(typeK);
					       $2->nodekind = statementK;
                           $2->kind.stmt = variableK;
                           $$->type = $1->type;
						   $2->type = $1->type;
                           $$->child[0] = $2;
						   $2->type = $1->type;
                 } 
	;	
compostodecl:		ACH localdeclaracoes statementlista FCH
                 {
                            YYSTYPE t = $2;
                            if(t != NULL)
						    {
                               while(t->sibling != NULL)
                                  t = t->sibling;
                                t->sibling = $3;
                                $$ = $2;
                            }
                            else
                               $$ = $3;
                  }
	;
localdeclaracoes:	localdeclaracoes varDeclaracao
                  {
                            YYSTYPE t = $1;
                            if(t != NULL)
							{
                            	while(t->sibling != NULL)
                                	 t = t->sibling;
                             	t->sibling = $2;
                             	$$ = $1;
                            }
                            else
                               $$ = $2;
                   }
	|			vazio
	;
statementlista:		statementlista statement
                   {
                           YYSTYPE t = $1;
                           if(t != NULL)
						   {
                              while(t->sibling != NULL)
                                   t = t->sibling;
                              t->sibling = $2;
                              $$ = $1;
                           }
                           else
                             $$ = $2;
                   }
	|			vazio
	;
statement:			expressaodecl
				{ $$ = $1; }
	|			compostodecl
				{ $$ = $1; }
	|			selecaodecl
				{ $$ = $1; }
	|			iteracaodecl
				{ $$ = $1; }
	|			retornodecl
				{ $$ = $1; }
	|			ERR
				{ 
					$$ = newExpNode(operationK);
                            		$$->attr.op = ERR;
						$$->type = voidK; 
				}
expressaodecl:		expressao PEV
				{ $$ = $1; } 
	|			PEV 
				{ }
	;
selecaodecl:		IF APR expressao FPR statement 
                   {
                             $$ = newStmtNode(ifK);
                             $$->child[0] = $3;
                             $$->child[1] = $5;
                   }

	|			IF APR expressao FPR statement ELSE statement
                   {
							 
                             $$ = newStmtNode(ifK);
                             $$->child[0] = $3;
                             $$->child[1] = $5;
                             $$->child[2] = $7;
                   }
	;
iteracaodecl:		WHILE APR expressao FPR statement
                   {
                             $$ = newStmtNode(whileK);
                             $$->child[0] = $3;
                             $$->child[1] = $5;
                   }
	;
retornodecl :		RETURN PEV
                   {
                            $$ = newStmtNode(returnK);
							$$->type = voidK;
                   } 
	| 	 		RETURN expressao PEV
                   {
                            $$ = newStmtNode(returnK);
                            $$->child[0] = $2;
                   }

	;
expressao:			var IGL expressao

                    {
                            $$ = newStmtNode(assignK);
                            $$->child[0] = $1;
                            $$->child[1] = $3;
                    } 
	| 			simplesexpressao
				{ $$ = $1; }
	;
var:				identificador
				{ $$ = $1; } 
	|			identificador ACO expressao FCO
	;
simplesexpressao:	somaexpressao relacional somaexpressao                       
				{
                            $$ = $1;
                            $$->child[0] = $3;
                            $$->kind.exp = vectorK;
							$$->type = integerK;
                }
 
	| 			somaexpressao
	;

relacional: 		MEIGL
                {
                            $$ = newExpNode(operationK);
                            $$->attr.op = MEIGL;                            
							$$->type = booleanK;
                }
	|			MEN
                {
                            $$ = newExpNode(operationK);
                            $$->attr.op = MEN;                            
							$$->type = booleanK;
                }
	|			MAI
                {
                            $$ = newExpNode(operationK);
                            $$->attr.op = MAI;                            
							$$->type = booleanK;
                }
	|			MAIGL
                {
                            $$ = newExpNode(operationK);
                            $$->attr.op = MAIGL;                            
							$$->type = booleanK;
                }
	|			IGLIGL
                 {
                            $$ = newExpNode(operationK);
                            $$->attr.op = IGLIGL;  
							$$->type = booleanK;                          
                 }

	|			DIF
                 {
                            $$ = newExpNode(operationK);
                            $$->attr.op = DIF;
							$$->type = booleanK;                            
                 }
	;
somaexpressao:		somaexpressao soma termo 
                 {
                            $$ = $2;
                            $$->child[0] = $1;
                            $$->child[1] = $3;
                 }
	| 			termo
				{ $$ = $1; } 
	;
soma:				SOM
                {
                            $$ = newExpNode(operationK);
                            $$->attr.op = SOM;                            
                } 
	| 			SUB
				{
                            $$ = newExpNode(operationK);
                            $$->attr.op = SUB;                            
                }
	;

termo:			termo mult fator
                {
                            $$ = $2;
                            $$->child[0] = $1;
                            $$->child[1] = $3;
                } 
	|	 		fator
				{ $$ = $1; } 
	;
mult:			MUL
                {
                            $$ = newExpNode(operationK);
                            $$->attr.op = MUL;                            
                }
	|			DIV
 				{
                            $$ = newExpNode(operationK);
                            $$->attr.op = DIV;                            
                }
	;
fator:			APR expressao FPR
				{ $$ = $2; }  
	|			var
				{ $$ = $1; } 
	|			ativacao
				{ $$ = $1; } 
	| 			numero
				{ $$ = $1; } 
	;
ativacao : 		identificador APR arglista FPR
                {
                            $$ = $1;
                            $$->child[0] = $3;
                            $$->nodekind = statementK;
                            $$->kind.stmt = callK;
                }
	;
arglista : 		arglista VIR expressao
                {
                            YYSTYPE t = $1;
                             if(t != NULL)
							 {
                                while(t->sibling != NULL)
                                   t = t->sibling;
                                 t->sibling = $3;
                                 $$ = $1;
                             }
                             else
                                 $$ = $3;
                 }
	|			expressao
				{ $$ = $1; } 
	|			vazio
	;
	
vazio: {		
	 $$ = NULL;
}
identificador :	ID
				{
                            $$ = newExpNode(idK);
                            $$->attr.name = copyString(tokenString);
                 }
    ;
numero :   		NUM
                {
                             $$ = newExpNode(constantK);
                             $$->attr.val = atoi(tokenString);
							 $$->type = integerK;
				}
    ;

%%

int yyerror(char* message){
    fprintf(listing,"Erro de Sintaxe na linha %d: %s\n",lineno,message);
    fprintf(listing,"Token atual: ");
    printToken(yychar,tokenString);
   Error = TRUE;
    return 0;
}


static int yylex(void){
    return getToken();
}

TreeNode * parse(void){
    yyparse();
    return savedTree;
}
