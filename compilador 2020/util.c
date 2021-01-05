/****************************************************/
/* File: util.c                                     */
/* Utility function implementation                  */
/* for the TINY compiler                            */
/* Compiler Construction: Principles and Practice   */
/* Daiana Santos e Isadora Muniz                    */
/****************************************************/

#include "globals.h"
#include "util.h"


/* Procedure printToken prints a token 
 * and its lexeme to the listing file
 */
void printToken( TokenType token, const char* tokenString )
{ 
  switch (token)
  { 
	case IF: fprintf(listing, "Palavra Reservada: %s\n",tokenString); break;
    case ELSE: fprintf(listing, "Palavra Reservada: %s\n",tokenString); break;
    case INT: fprintf(listing, "Palavra Reservada: %s\n",tokenString); break;
    case RETURN: fprintf(listing, "Palavra Reservada: %s\n",tokenString); break;
    case VOID: fprintf(listing, "Palavra Reservada: %s\n",tokenString); break;
    case WHILE: fprintf(listing, "Palavra Reservada: %s\n",tokenString); break;
    case IGL: fprintf(listing,"Simbolo: =\n"); break;
    case MEN: fprintf(listing,"Simbolo: <\n"); break;
    case IGLIGL: fprintf(listing,"Simbolo: ==\n"); break;
    case MAI: fprintf(listing,"Simbolo: >\n"); break; 
    case MEIGL: fprintf(listing, "Simbolo: <=\n"); break;
    case MAIGL: fprintf(listing, "Simbolo: >=\n"); break; 
    case DIF: fprintf(listing, "Simbolo: !=\n"); break;
    case ACO: fprintf(listing, "Simbolo: [\n"); break;
    case FCO: fprintf(listing, "Simbolo: ]\n"); break;
    case ACH: fprintf(listing, "Simbolo: {\n"); break;
    case FCH: fprintf(listing, "Simbolo: }\n"); break;	
    case APR: fprintf(listing,"Simbolo: (\n"); break;
    case FPR: fprintf(listing,"Simbolo: )\n"); break;
    case PEV: fprintf(listing,"Simbolo: ;\n"); break;
    case VIR: fprintf(listing,"Simbolo: ,\n"); break;
    case SOM: fprintf(listing,"Simbolo: +\n"); break;
    case SUB: fprintf(listing,"Simbolo: -\n"); break;
    case MUL: fprintf(listing,"Simbolo: *\n"); break;
    case DIV: fprintf(listing,"Simbolo: /\n"); break;
    case ENDFILE: fprintf(listing,"EOF\n"); break; 
    case NUM:
      fprintf(listing, "Numero: %s\n",tokenString);
      break;
    case ID:
      fprintf(listing, "Identificador: %s\n",tokenString);
      break;
    case ERR:
      fprintf(listing, "Erro: %s\n",tokenString);
      break;
    default: 
      fprintf(listing,"Token desconhecido: %d\n",token);
  }
}

void aggScope(TreeNode* t, char* scope)
{
	int i;
	while(t != NULL)
	{
		for(i = 0; i < MAXCHILDREN; ++i)
		{
			t->attr.scope = scope;
			aggScope(t->child[i], scope);
		}
		t = t->sibling; 
	}
}


/* Function newStmtNode creates a new statement
 * node for syntax tree construction
 */

TreeNode * newStmtNode(StatementKind kind)
{ 
    TreeNode * t = (TreeNode *) malloc(sizeof(TreeNode));
    int i;
    if (t==NULL)
        fprintf(listing,"Erro de acesso de memoria na linha %d\n",lineno);
    else 
    {
        for (i=0;i<MAXCHILDREN;i++) 
            t->child[i] = NULL;
        t->sibling = NULL;
        t->nodekind = statementK;
        t->kind.stmt = kind;
        t->lineno = lineno;
        t->attr.scope = "global";
    }
    return t;
}

/* Function newExpNode creates a new expression 
 * node for syntax tree construction
 */

TreeNode * newExpNode(ExpressionIdentifier kind)
{ 
    TreeNode * t = (TreeNode *) malloc(sizeof(TreeNode));
    int i;
    if (t==NULL)
        fprintf(listing,"Erro de acesso de memoria na linha %d\n",lineno);
    else 
    {
        for (i=0;i<MAXCHILDREN;i++) 
            t->child[i] = NULL;
        t->sibling = NULL;
        t->nodekind = expressionK;
        t->kind.exp = kind;
        t->lineno = lineno;
        t->type = VOID;
        t->attr.scope = "global";
    }
    return t;
}

/* Function copyString allocates and makes a new
 * copy of an existing string
 */

char * copyString(char * s)
{ 
    int n;
    char * t;
    if (s==NULL) 
        return NULL;
    n = strlen(s)+1;
    t = malloc(n);
    if (t==NULL)
        fprintf(listing,"Erro de acesso de memoria na linha %d\n",lineno);
    else 
        strcpy(t,s);
    return t;
}

/* Variable indentno is used by printTree to
 * store current number of spaces to indent
 */

static int indentno = 0;

/* macros to increase/decrease indentation */
#define INDENT indentno+=2
#define UNINDENT indentno-=2

/* printSpaces indents by printing spaces */
static void printSpaces(void)
{ 
  int i;
    for (i=0;i<indentno;i++)
      fprintf(listing," ");
}

/* procedure printTree prints a syntax tree to the 
 * listing file using indentation to indicate subtrees
 */
void printTree( TreeNode * tree )
{ int i;
  INDENT;
  while (tree != NULL) {
    printSpaces();
    if (tree->nodekind==statementK)
    { switch (tree->kind.stmt) {
        case ifK:
          fprintf(listing,"If\n");
          break;
        case assignK:
          fprintf(listing,"Atribuicao\n");
          break;
		case whileK:
	 	 fprintf(listing,"While\n");
	 	 break;
		case variableK:
	 	 fprintf(listing,"Variavel %s\n", tree->attr.name);
	 	 break;
		case functionK:
	 	 fprintf(listing,"Funcao %s\n", tree->attr.name);
	  	break;
        case callK:
	 	 fprintf(listing,"Chamada para funcao %s \n", tree->attr.name);
	  	break;        
		case returnK:
	 	 fprintf(listing, "Return\n");
	 	 break;        
	        
		default:
          fprintf(listing,"Tipo ExpNode desconhecido\n");
          break;
      }
    }
    else if (tree->nodekind==expressionK)
    { switch (tree->kind.exp) {
        case operationK:
          fprintf(listing,"Operacao: ");
          printToken(tree->attr.op,"\0");
          break;
        case constantK:
          fprintf(listing,"Constante: %d\n",tree->attr.val);
          break;
        case idK:
          fprintf(listing,"Id: %s\n",tree->attr.name);
          break;
		case vectorK:
          fprintf(listing,"Vetor: %s\n",tree->attr.name);
          break;
		case vectorIdK:
          fprintf(listing,"Indice [%d]\n",tree->attr.val);
          break;
		case typeK:
          fprintf(listing,"Tipo %s\n",tree->attr.name);
          break;
        
        default:
          fprintf(listing,"Tipo ExpNode desconhecido\n");
          break;
      }
    }
    else fprintf(listing,"Tipo de no desconhecido\n");
    for (i=0;i<MAXCHILDREN;i++)
         printTree(tree->child[i]);
    tree = tree->sibling;
  }
  UNINDENT;
}
