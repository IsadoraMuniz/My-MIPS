%{

#include "globals.h"
#include "util.h"
#include "scan.h"

char tokenString[MAXTOKENLEN+1];
static int yylex(void);
%}

DIGITO [0-9]
LETRA [a-zA-Z]
OUTRO [^0-9a-zA-Z;/=\-"+""*""("")"]
COINI "/*"
COFI "*/"
%%


int				return INT;
while				return WHILE;
return				return RETURN;
if				return IF;
else				return ELSE;
void				return VOID;


{COINI}({LETRA}|{DIGITO}|{OUTRO})*({COFI})* {}
{DIGITO}+		return NUM;
"+"				return SOM;
"-"				return SUB;
"*"				return MUL;
"/"				return DIV;
"<"				return MEN;
">"				return MAI;
"="				return IGL;
"=="			return IGLIGL;
"<="			return MEIGL;
">="			return MAIGL;
"!="			return DIF;
";"				return PEV;
","				return VIR;
"("				return APR;
")"				return FPR;
"["				return ACO;
"]"				return FCO;
"{"				return ACH;
"}"				return FCH;
{LETRA}({LETRA}|{DIGITO})*	return ID;
\n				{lineno++;}
\t				{}
[ ]+			{}
.				return ERR;
				
%%

TokenType getToken(void)
{ 
      static int firstTime = TRUE;
      TokenType currentToken;
      if (firstTime)
      { 
        firstTime = FALSE;
        lineno++;
        yyin = source;
        yyout = listing;
      }
      currentToken = yylex();
      strncpy(tokenString,yytext,MAXTOKENLEN);
      if (TraceScan) 
      {
        fprintf(listing,"\t%d: ",lineno);
        printToken(currentToken,tokenString);
      }
      return currentToken;
}


