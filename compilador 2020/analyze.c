/****************************************************/
/* 					                                */
/* Analisador Semantico                             */
/* Daiana Santos e Isadora Muniz                    */
/****************************************************/

#include "globals.h"
#include "symtab.h"
#include "analyze.h"

static void typeError(TreeNode * t, char * message)
{ fprintf(listing,"Erro de tipo em %s na linha %d: %s\n",t->attr.name, t->lineno,message);
  Error = TRUE;
}

/* counter for variable memory locations */
static int location = 2;

/* Procedure traverse is a generic recursive 
 * syntax tree traversal routine:
 * it applies preProc in preorder and postProc 
 * in postorder to tree pointed to by t
 */
static void traverse( TreeNode * t,
               void (* preProc) (TreeNode *),
               void (* postProc) (TreeNode *) )
{ 

	if (t != NULL)
 	{ 
		preProc(t);
    	{ 
	  		int i;
	        for (i=0; i < MAXCHILDREN; i++)
        		traverse(t->child[i],preProc,postProc);
    	}
    	postProc(t);
    	traverse(t->sibling,preProc,postProc);
  	}
}

/* nullProc is a do-nothing procedure to 
 * generate preorder-only or postorder-only
 * traversals from traverse
 */
static void nullProc(TreeNode * t)
{ 
	if (t==NULL) 
		return;
  	else 
		return;
}

/* Procedure insertNode inserts 
 * identifiers stored in t into 
 * the symbol table 
 */
static void insertNode( TreeNode * t)
{ 
	
	switch (t->nodekind)
    { 

		case statementK:
      	switch (t->kind.stmt)
        { 		
			case variableK:
          		if (st_lookup(t->attr.name, t->attr.scope) == -1 && st_lookup(t->attr.name, "global") == -1)
            		st_insert(t->attr.name,t->lineno,location++, t->attr.scope, "variable", "integer");
          		else
            		typeError(t,"Erro semantico: Declaracao Invalida. Ja declarada.");	
            break;
			case functionK:
				if (st_lookup(t->attr.name, t->attr.scope) == -1 && st_lookup(t->attr.name, "global") == -1)
				{
					if(t->type == integerK)
                        st_insert(t->attr.name,t->lineno,location++, t->attr.scope,"function", "integer");
            		else
                        st_insert(t->attr.name,t->lineno,location++, t->attr.scope,"function", "void");
            	}
          		else
            		typeError(t,"Erro semantico: Declaracao Invalida. Ja declarada.");	
			break;
			case callK:

				if (st_lookup(t->attr.name, t->attr.scope) == -1 && st_lookup(t->attr.name, "global") == -1 )
            		typeError(t,"Erro semantico: Chamada Invalida. Nao foi declarada.");	

          		else
            		st_insert(t->attr.name,t->lineno,location++, t->attr.scope, "call", "-");
			case returnK:
          	break;
        	default:
          break;
      }
      break;
      case expressionK:
      switch (t->kind.exp)
      { 
		  case idK:
			if (st_lookup(t->attr.name, t->attr.scope) == -1 && st_lookup(t->attr.name, "global") == -1)
				 typeError(t,"Erro semantico : variavel nao foi declarado");
			else
            	st_insert(t->attr.name,t->lineno,0, t->attr.scope, "variable", "integer");	
	      break;
	      case vectorK:
	      	if (st_lookup(t->attr.name, t->attr.scope) == -1 && st_lookup(t->attr.name, "global") == -1)
				 typeError(t,"Erro semantico : nao foi declarado");
			else
            	st_insert(t->attr.name,t->lineno,0, t->attr.scope, "vector", "integer");	
		  break;
		  case vectorIdK:
		  	if (st_lookup(t->attr.name, t->attr.scope) == -1 && st_lookup(t->attr.name, "global") == -1)
				 typeError(t,"Erro semantico : nao foi declarado");
			else
            	st_insert(t->attr.name,t->lineno,0, t->attr.scope, "vector index", "integer");	
		  case typeK:
          break;
        default:
        break;
      }
      break;
    default:
      break;
  }
}

void buildSymtab(TreeNode * syntaxTree)
{ 
  st_insert("input",0,0,"global", "function", "integer"); 
  st_insert("output",0,1,"global", "function", "integer");   
  traverse(syntaxTree,insertNode,nullProc); 
  if(st_lookup("main", "global") == -1)
   {
   		printf("main nao foi declarada");
   		Error = TRUE; 
   }
	
  if (TraceAnalyze)
  { 
    fprintf(listing,"\nTabela de simbolos:\n\n");
    printSymTab(listing);
  }
}

static void checkNode(TreeNode * t)
{ switch (t->nodekind)
  { 
   case expressionK:
      switch (t->kind.exp)
      { 
        case operationK:
        break;
        default:
        break;
      }
      break;
    case statementK:
      switch (t->kind.stmt)
      { 
        case ifK:
          if (t->child[0]->type == integerK && t->child[1]->type == integerK)
           typeError(t->child[0],"se teste nao eh Boolean");
        break;
        case assignK:
          if (t->child[0]->type == voidK || t->child[1]->type == voidK)
            typeError(t->child[0],"atribuicao de valor nao-inteiro");
		  else if(t->child[1]->kind.stmt == callK)
		  {
		    if(strcmp(st_lookup_type(t->child[1]->attr.name, "global"), "void")==0)
				typeError(t->child[1],"atribuicao de returno vazio");
			}
        break;
        default:
        break;
      }
    break;
    default:
    break;
  }
}

void typeCheck(TreeNode * syntaxTree)
{ 
    traverse(syntaxTree,nullProc,checkNode);
}
