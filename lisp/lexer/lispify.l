/* lexer: -*- mode: c -*-
/* {{{  Section 1: */

%{
 /* {{{  Modification history */

 /* Modified: Sun Mar  7 11:22:41 EST 1993*/
/* handling chapter as another sectioning construct.*/
 /* Modified: Sat Mar  6 17:32:52 EST 1993*/
/* handling include files */
  /*Written by: T. V. Raman <raman@cs.cornell.edu> */
  /* Modification history */
 /* Modified:   Wed Aug  5 09:29:42 EDT 1992*/
 /*Switched to using flex instead of lex*/
 /* There is a bug in lex which results in generating incorrect C */
 /* programs if  there are comments between rules, and this means that */
 /* inserting fold marks breaks lex*/
 /* Modified:   Wed May  6 21:12:29 EDT 1992*/
 /* adding patterns BEGIN_ENV_OPEN and BEGIN_ENV_CLOSE  */
 /* Modified:   Mon May  4 12:36:43 EDT 1992*/
 /* skip lines containing vskip */
 /* Modified:   Mon May  4 12:17:54 EDT 1992*/
 /* Recognizeslide environment */
 /* Modified: Sun May  3 17:20:56 EDT 1992*/
 /* Make lexer discard eject and maketitle commands for the present*/
 /* Modified:   Thu Apr 30 20:25:10 EDT 1992*/
 /* make lexer discard backslash comma for the present */
 /* Modified:   Wed Apr 29 15:47:03 EDT 1992*/
 /* handled [ ] eg. optional args in latex */
  /* This still needs to be done properly */
 /* Modified:   Wed Apr 29 08:32:37 EDT 1992*/
 /* Leave explicit paragraph breaks at the end of environments like */
 /* lists */
 /* Modified:   Tue Apr 28 16:32:51 EDT 1992*/
 /* Handle backslash space and section * */
 /* Modified:   Sat Feb 29 11:07:04 EST 1992*/
 /* introduce a new state for parsing tex macros */
 /* Modified:   Thu Feb 27 10:19:13 EST 1992*/
 /* In math mode strings of digits with optional point form a number */
 /* Modified:   Sun Feb 23 19:14:40 EST 1992*/
 /* Italic correction now handled by lexer */
  /* lexer can now be called from lisp*/
 /* Modified:   Sat Feb 15 14:29:02 EST 1992*/
 /* added code to handle unknown environments */
  /* added code to handle quotes and unmatched quotes */
 /* Modified:   Thu Feb 13 19:28:26 EST 1992*/
 /* Divided file into sections to make updates easier */
 /* Modified:   Thu Feb 13 11:56:32 EST 1992*/
 /* make math mode completely independent of text mode */
  /* Hence block in text mode is now math-block */ 
  /* Jan 24: Making lispify a smart tokeniser */
 /* Modified:   Tue Feb  4 12:57:24 EST 1992*/
 /*  adding start condition for math mode */
 /* Modified: Fri Aug 28 13:09:37 EDT 1992*/
/* handle verbatim environment */
 /* Modified: Thu Sep  3 12:27:21 EDT 1992*/
/* Pass through '^' in text since it may be inside \verb|| */
 /* Modified: Tue Sep  8 20:46:10 EDT 1992*/
/* skip pictures */
 /* Modified: Wed Sep  9 09:34:42 EDT 1992*/
/* added function escape quotes to handle quote in comment */
 /* Modified: Mon Sep 14 13:11:08 EDT 1992*/
/* Convert standard abbreviations so dectalk does not think it has */
 /* reached the end of a sentence. Think of a better place to do such */
 /* things, ideally they do not belong in the lexer */
 /* Modified: Mon Sep 21 17:04:14 EDT 1992*/
/* Introduced rules to catch \( \) for inline math and \[ \] for */
 /* display math in latex */
 /* Modified: Mon Sep 28 08:59:59 EDT 1992*/
/* Make lexer echo spaces in math mode */
 /* Modified: Wed Sep 30 09:32:01 EDT 1992*/
/* undid above it is a kluge */
/* introduce accents in text in the lexer */
 /* Modified: Sat Oct  3 19:36:32 EDT 1992*/
/* handle Latex eqalign and case environments */
 /* Modified: Sun Oct  4 08:35:52 EDT 1992*/
/* Adding variable brace_count to keep track of matching braces and */
 /* then setting up rules for \hbox and \mbox that save the state of */
 /* the automaton  enter text mode, and then return to the saved mode */
 /* after the matching brace is seen. For the present saving state in */
 /* a variable rather than on a stack as this seems sufficient. It */
 /* will not handle a horribly hairy example like
    $x+\hbox{foo $a\hbox{doo}$}$  but there is no need to use an inner
    hbox here.*/
 /* Modified: Sat Oct 10 19:47:50 EDT 1992*/
/* Rationalized how arrays and tables are handled. Elements of arrays */
 /* no longer forced into math mode. Rely on enclosing math brackets */
 /* Modified:    Sat Oct 10 19:53:15 EDT 1992*/
/*Introducing list_env variable that keeps track of list environments */
 /*so that it can wanr user about an item that occurs outside a lit */
 /*environment. */
/* If this not present, it is difficult to track down a nasty */
 /* mismatched list error that results in the parsing when a new list */
 /* environment occurs */ 
 /* Modified:     Sat Oct 10 19:47:20 EDT 1992*/
/*Added some validation of latex input */
 /* Modified: Sat Oct 31 19:11:35 EST 1992*/
/* mbox and hbox set up to be handled as user defined macros */
 /* Modified: Thu Dec 24 15:23:53 EST 1992*/
/* removing whitespace from env open so that a list or a table does */
 /* not become part of the preceding paragraph.  Here is the old */
 /* definition of  the pattern:*/
    /* BEGIN_ENV_OPEN ({WHITESPACE})*{BACKSLASH}"begin{" */
 /* Modified: Wed Apr  7 10:10:23 EDT 1993*/
/* handling accents as characters */
 /* end of modification history */
  /* includes and macros */

 /* }}} */
 /* {{{  C Preprocessor statements */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <malloc.h>
#define QUIT_ON_INCLUDE_FAIL 0
#define MAXLEN 80
#define IS_MATH()  ((inline_math_flag || display_math_flag))
#define  END_LIST() (printf(")\n"))
#define  DISPLAY_MATH  10
#define INLINE_MATH 20
#define PRINT_AS_STRING(a) printf(" \"%s\" ", a)
#define MAX_INCLUDE_DEPTH 10
YY_BUFFER_STATE include_stack[MAX_INCLUDE_DEPTH];
int include_stack_ptr = 0;

 /* }}} */
 /* {{{  Global C variables */

  /*global variables */
int chapter_flag = 0;
int section_flag=0;
int subsection_flag=0;
int subsubsection_flag = 0;
int aparagraph_flag = 0;
int inline_quote_flag = 0;
int inline_math_flag =0;
int display_math_flag = 0;
int table_flag = 0;
int array_flag = 0;
int saved_math_state = 0;
int brace_count = 0; 
int list_environment_flag = 0;

 /* }}} */
 /* {{{  C function declarations */

 /* Function declarations */
char *  latex_env_name();
char * escape_quotes(); 
int clean_up_quotes();
char * get_macro_name();
char * get_tex_filename();
char * get_tex_filename_without_braces();
int list_env_p(); 

 /* }}} */
%}
 /* {{{  Size of automaton */

%e 2500
%P 25000
%n 10000
%a 25000
%O 25000

 /* }}} */
 /* {{{  Lexical definitions */

CONTROL-M [\015]
CONTROL-L [\014]
ALPHABET  [a-zA-Z]
ACCENT_ACUTE "\\'"
ACCENT_GRAV  "\\`"
ACCENT_UMLAUT  "\\\"" 
ACCENT_CIRCUMFLEX "\\^"
ACCENT ({ACCENT_CIRCUMFLEX}|{ACCENT_GRAV}|{ACCENT_ACUTE}|{ACCENT_UMLAUT})
FILENAME_CHAR [a-zA-Z]
LETTER ([a-zA-Z-])|({ACCENT}) 
DIGIT [0-9]
NUMBER {DIGIT}+
MATH_NUMBER  {DIGIT}*(".")?{NUMBER}
TEXT_NUMBER {NUMBER}((",")?{MATH_NUMBER})*
TIME_NUMBER {DIGIT}+(":")?{DIGIT}+
ALPHABET_STRING ({LETTER})*
WHITESPACE [ \t\n]
H_SPACE [ \t]
NEWLINE [\n]
BLANK_LINE (({H_SPACE})*{NEWLINE})
PARAGRAPH_SEPARATOR {NEWLINE}({BLANK_LINE})+ 
PUNCT [-=_^/.,:;!?"()@\[\]|<>*+'`]
BACKSLASH \\
LATEX_SPECIAL_CHAR [#$%&~_^{}\\]
MATH_OP [+*/'<>=-]
POS_TAB_ARG "["[hbtp]+"]" 
OPT_TAB_ARG "{"[lrc|]*"}"
BEGIN_ENV_OPEN {BACKSLASH}"begin"({H_SPACE})*"{"
BEGIN_ENV_CLOSE "}"({WHITESPACE})*
BEGIN_DISPLAY_MATH  "$$"
END_DISPLAY_MATH "$$"

 /* }}} */
%S MATH MACRO  PICTURE
%x INCLUDE 
%%

 /* }}} */
 /* {{{ Error checking: */

"\\end "{H_SPACE}* {
  fprintf(stderr, "space after \\end can cause problems \n");
  exit(1);
}

 /* }}} */
 /* {{{  include files */

"\\input"              BEGIN(INCLUDE);
"\\include" BEGIN(INCLUDE);
<INCLUDE>[ \t]*    ;  /* eat the whitespace */
<INCLUDE>"{"[^}\t\n ]*"}" {/* get file name */
 char * filename;
FILE *infile; 
filename = (char *) malloc(MAXLEN);
get_tex_filename(filename, yytext); 
if (!filename) {
  fprintf(stderr, "Error: could not get name of include file from %s \n",
          yytext);
  exit(1);
}
  if ( include_stack_ptr >= MAX_INCLUDE_DEPTH )
  {
    fprintf( stderr, "Includes nested too deeply" );
    exit( 1 );
  }
  
  infile = fopen( filename, "r" );
  if ( ! infile ){
    fprintf(stderr,
            "Could not open include file %s  \n", filename);
    if (QUIT_ON_INCLUDE_FAIL){
      exit(1);
    }
    else{ 
BEGIN(0);
}
  }
else{
  /* open successful */ 
  include_stack[include_stack_ptr++] =
    YY_CURRENT_BUFFER;
yyin = infile; 
  yy_switch_to_buffer(
                      yy_create_buffer( yyin, YY_BUF_SIZE ) );
fprintf(stderr, "Reading input from include file %s \n", filename); 
  BEGIN(INITIAL);}
}

<INCLUDE>{FILENAME_CHAR}+  {/* get file name */
 char * filename;
FILE *infile; 
filename = (char *) malloc(MAXLEN);
 fprintf(stderr, "fired \n");
get_tex_filename_without_braces(filename, yytext); 
if (!filename) {
  fprintf(stderr, "Error: could not get name of include file from %s \n",
          yytext);
  exit(1);
}
  if ( include_stack_ptr >= MAX_INCLUDE_DEPTH )
  {
    fprintf( stderr, "Includes nested too deeply" );
    exit( 1 );
  }
  
  infile = fopen( filename, "r" );
  if ( ! infile ){
    fprintf(stderr,
            "Could not open include file %s  \n", filename);
    if (QUIT_ON_INCLUDE_FAIL){
      exit(1);
    }
    else{ 
BEGIN(0);
}
  }
else{
  /* open successful */ 
  include_stack[include_stack_ptr++] =
    YY_CURRENT_BUFFER;
yyin = infile; 
  yy_switch_to_buffer(
                      yy_create_buffer( yyin, YY_BUF_SIZE ) );
fprintf(stderr, "Reading input from include file %s \n", filename); 
  BEGIN(INITIAL);}
}

<<EOF>> {
  if ( --include_stack_ptr < 0 )
  {
    yyterminate();
  }
  else
    yy_switch_to_buffer(
                        include_stack[include_stack_ptr] );
}

 /* }}} */
 /* {{{ Dectalk abbreviations */

 /* {{{ Months of the year */

"Jan." |
"Feb." |
"Mar." |
"Apr." |
"Aug." |
"Sep." |
"Oct." |
"Nov." |
"Dec." {
PRINT_AS_STRING(yytext); 
}

 /* }}} */
 /* {{{ ie and eg */

  
"i.e." {
  printf(" (cs \"ie\")");
}
"e.g." {
  printf("(cs \"eg\")");
 }

 /* }}} */
 /* {{{ Misc abbreviations  */

"Corp." | 
"etc." {
PRINT_AS_STRING(yytext); 
}

 /* }}} */
 /* {{{  Initials */

{ALPHABET}"."  {
  /* letter followed by a period probably an initial */
PRINT_AS_STRING(yytext);
}

 /* }}} */
 /* {{{ Salutations  */

"Prof." |
"Mr." |
"Mrs." |
"St." |
"PhD." |
"Phd." |
"Dr." {
PRINT_AS_STRING( yytext);
}

 /* }}} */
 /* {{{ Days of the week */

"Sun." |
"Mon." |
"Tue." |
"Wed." |
"Thu." |
"Fri." |
"Sat." {
PRINT_AS_STRING(yytext); 
}  

 /* }}} */

 /* }}} */
 /* {{{ tex commands that are stripped  */
"\\@" {printf ("(cs \"@\" )\n"); }
"\\hspace*" ;
"\\bigskip" ;
"\\noalign" ;
"\\bigl" ;
"\\bigr" ;
"\\biggl" ;
"\\biggr" ;
"\\bigggl" ;
"\\bigggr" ;
"\\ul" {
  printf(" (cs \"em\" ) ");
}
"\\thispagestyle{"[^}]*"}"  ;
"\\medskip" ;
"\\advance" ;
"\\leftskip" ;
"\\rightskip" ;
"by"[0-9]+"pc"  ;
"by"[0-9]+"pt" ;
"by"[0-9]+"in" ;
"\\protect" ;
"\\smallskip" ;
"\\displaystyle" ;
"\\left" ;
"\\right" ;
"\\boldmath" ;
"\\unboldmath" ;
"\\raggedright" ;
"\\vfil" ;
"\\vfill" ;
"\\hfil" ;
"\\hfill" ;
"\\scriptstyle" ;
"\\vskip"{H_SPACE}*([0-9]+"pt")? ;
"\\hspace*"{H_SPACE}*([0-9]+"in")? ;
"\\vspace"("*")?"{"{DIGIT}+"pt}" ;
"\\vspace"("*")?"{"{DIGIT}+"in}" ;
"\\kern"{H_SPACE}*([0-9]+"pt")? ;
"\\eject" ;
"\\maketitle" ;
"\\noindent" ;
"\\goodbreak" ;
"\\," ;
"\\!" ;
"\\:" ;
"\\;"  ; 
"\\hline" ;
"\\ " ;
"\\/" ;
"\\mathstrut" ;
"\\pagebreak" ;
"\\linebreak" ;

 /* }}} */
 /* {{{  Some special verb commands */

"\\verb|[|" printf("\"[\"");
"\\verb|{|"   printf("\"{\"");

 /* }}} */
 /* {{{ special tex and latex characters */

"--" {
PRINT_AS_STRING(yytext);
}

"---" {
PRINT_AS_STRING(yytext);
}

"~" {
  printf("  ");
}

"\\cr" |
"\\headrow" |
"\\newrow" | 
{BACKSLASH}{BACKSLASH}  {
  if (  table_flag) {
    /* end current array element */
    END_LIST();
    /* End current row */
    END_LIST();
    /* Now start next row */
    printf("( ");
    /* and start its first element */
    printf("( ");
  }
  else if(array_flag){
 /* Modified: Tue Oct  6 17:06:17 EDT 1992*/
/* Not marking each element to be inline-math*/
    /* end current array element */
    END_LIST();
    /* End current row */
    END_LIST();
    /* Now start next row */
    printf("( ");
    /* and start its first element */
    printf("( ");
  }
  else{
    printf(" (newline)  ");
  }
};

{BACKSLASH}{LATEX_SPECIAL_CHAR} {
  /* print it after stripping off the backslash */
  printf(" \"%s\"",escape_quotes( (yytext + 1)));};

\& {
  if (array_flag) {
 /* Modified: Tue Oct  6 17:06:17 EDT 1992*/
/* Not marking each element to be inline-math*/
    /* end an array element */
    END_LIST();
    /* and begin a new one */
    printf (" (   ");
  }
  else if (table_flag) {
    /* end a table  element */
    END_LIST();
    /* and begin a new one */
    printf (" (   ");
  }
  else {
 /* Modified:   {Mon Sep 27 19:07:30 EDT 1993*/
    /* output field separator so that we can handle tex matrix command */
printf(" (field-separator) "); }
};

<MATH>"^" printf("\"^\"");
<MATH>"_"  printf("\"_\"");

 /* }}} */
 /* {{{ skip pictures */

{BEGIN_ENV_OPEN}"picture"{BEGIN_ENV_CLOSE} {
  BEGIN PICTURE;
}
({WHITESPACE})*{BACKSLASH}"end{picture}"({H_SPACE})*  {
  BEGIN 0;
}

<PICTURE>.* ;

 /* }}} */
 /* {{{ kluges for handling macro definitions */

{BACKSLASH}"def"({WHITESPACE})*{BACKSLASH}({LETTER})* {
 /* handle macro definitions */
printf("(cs \"def\"  \"%s\"  ) ",get_macro_name(yytext));
 /* start arg list */
printf("(arglist ");
BEGIN MACRO;
}
\#{DIGIT} {
  /* Convert #1 to arg1 etc. */
  printf(" \"arg%c\" ", yytext[1]);
};
<MACRO>"{" {
    /* ending of arg list seen so mark it and remember */
    printf(" )");
    printf("(block ");
BEGIN 0;
}

 /* }}} */
 /* {{{  braces start groups */

<MATH>"{" {
  brace_count++;  /* count braces */
  printf("(subformula  "); 
};

"{" {
  brace_count++;  /* count braces*/
  printf("(block "); 
};

"}" {
  brace_count--; /* count braces */
  END_LIST();
  if (brace_count == 0) {
  /* reset state to math if necessary */
  if (saved_math_state) {
    switch (saved_math_state) {
    case INLINE_MATH: 
        inline_math_flag = 1;
      saved_math_state = 0;
      BEGIN MATH;
      break;
      case DISPLAY_MATH: 
        display_math_flag = 1;
        BEGIN MATH;
        saved_math_state = 0;
        break;
      default:
        fprintf(stderr, "Unknown saved_math_state %d \n", saved_math_state);
        exit(1); 
      }
  }
}
};

 /* }}} */
 /* {{{  hbox and mbox change state */

{BACKSLASH}"fbox {" |
{BACKSLASH}"hbox {" |
{BACKSLASH}"mbox {" |
{BACKSLASH}"hbox"({H_SPACE})*"{"  |
{BACKSLASH}"mbox"({H_SPACE})*"{"  {
  brace_count = 1; /*reset brace count */
  /* when brace_count reaches 0 we have seen the matching close brace */
  /* and can close the hbox */
  /* both hbox and mbox marked as mbox and will be processed as if */
  /* they were user defined macros ie: using define-text-object */ 
    clean_up_quotes();
  /* First set up state */
  if (display_math_flag) {
    display_math_flag = 0;
    inline_math_flag = 0; /*kluge*/
    saved_math_state = DISPLAY_MATH;
    BEGIN 0;
  }
  else if (inline_math_flag) {
    saved_math_state = INLINE_MATH;
    inline_math_flag = 0;
    display_math_flag = 0; /*kluge*/ 
    BEGIN 0;
  }
  printf("(cs \"mbox\"  )" ); 
  printf("(block "); 
}

  /* }}} */
 /* {{{  Brackets in latex */

<MATH>"[" {
  printf(" \"[\" ");
}

<MATH>"]" {
  printf(" \"]\" ");
}

"[" {
  /* optional args in a block */
  printf("(block \n ");
}

"]" {
  END_LIST();
}

 /* }}} */
 /* {{{  paragraph breaks */

{BACKSLASH}"par"  |
{BACKSLASH}"paragraph" |
{PARAGRAPH_SEPARATOR}  {
 /* dont put parbreaks inside math */
    
 if (!IS_MATH()) {
  clean_up_quotes();
  /* paragraph delimiter is a newline followed by an arbitrary number
     blank lines, where a blank is defined as a line with an arbitrary amount
     of optional h-space followed by a newline. */
  /* close apar if one opened */
  if (aparagraph_flag) {
    END_LIST();
    aparagraph_flag=0;}
  printf("\n parbreak \n");
}
};

 /* }}} */
 /* {{{  Beginning and ending math mode */

"\\[" {
  clean_up_quotes();
  display_math_flag= 1;
  printf("(display-math "); 
  BEGIN MATH;
  }

"\\]" { 
  display_math_flag = 0;
  END_LIST();
  BEGIN 0;
}

"$$" {
  if (inline_math_flag==1) {
    fprintf(stderr, "display math started inside inline math?\
Probably an inline math was closed and immediately opened.\
Check the latex file \n");
    exit(1); 
  }
  if(!display_math_flag) {
     clean_up_quotes();
    display_math_flag= 1;
    printf("(display-math "); 
    BEGIN MATH;
  }
  else {
    display_math_flag = 0;
    END_LIST();
    BEGIN 0;
  }
};

"\\(" {
  /*   clean_up_quotes();*/
  inline_math_flag= 1;
  printf("(inline-math "); 
  BEGIN MATH; 
  };

"\\)" {
  inline_math_flag = 0;
  END_LIST();
  BEGIN 0; 
}
"$" {
  if(!inline_math_flag) {
    /*     clean_up_quotes(); */
     inline_math_flag= 1;
    printf("(inline-math "); 
    BEGIN MATH; 
  }
  else {
    inline_math_flag = 0;
    END_LIST();
    BEGIN 0; 
  }
};

 /* }}} */
 /* {{{  math operators */

<MATH>"'" {
  /* catch single quote in math mode */
  printf("\"prime\"");
};

<MATH>"''"{H_SPACE}*  {
/* catch double prime in math mode */
  printf("\"double-prime\"");
};

<MATH>{MATH_OP}  {
PRINT_AS_STRING(yytext);
};

 /* }}} */
 /* {{{  tex comment */

"%".*  {
  /* Latex comment runs to end of line */
  /* Modified: Tue May  4 13:34:10 EDT 1993*/
 /* not doing anything with comments, so throw them away */
  /* 
  printf("(comment "); 
  printf("\" %s \"",  escape_quotes(yytext));
  END_LIST();
  */
  ;
  /* throw away comment */ 
};

 /* }}} */
 /* {{{  begin various environments */

 /* {{{  begin document */

{BEGIN_ENV_OPEN}"document"{BEGIN_ENV_CLOSE} {
  printf("(document "); 
}

 /* }}} */
 /* {{{ begin abstract  */

{BEGIN_ENV_OPEN}"abstract"{BEGIN_ENV_CLOSE} {
  clean_up_quotes();
  printf("(abstract "); 
}

 /* }}} */
 /* {{{ begin center */

{BEGIN_ENV_OPEN}"center"{BEGIN_ENV_CLOSE} {
  clean_up_quotes(); 
  printf("(center "); 
}

 /* }}} */
 /* {{{  begin quote */

{BEGIN_ENV_OPEN}"quote"{BEGIN_ENV_CLOSE} {
  clean_up_quotes();
  printf("(quote  ");
}

 /* }}} */
 /* {{{  begin quotation */

{BEGIN_ENV_OPEN}"quotation"{BEGIN_ENV_CLOSE} {
  clean_up_quotes();
  printf("(quotation  ");
}

 /* }}} */
 /* {{{  begin verbatim */

{BEGIN_ENV_OPEN}"verbatim"{BEGIN_ENV_CLOSE} {
  clean_up_quotes();
  printf("(verbatim  ");
}

 /* }}} */
 /* {{{  begin description */

{BEGIN_ENV_OPEN}"description"{BEGIN_ENV_CLOSE} {
  clean_up_quotes();
  list_environment_flag++; 
  printf("(description  ");
  /* Generate dummy item */
  /* this is to allow \item to be handled cleanly */
  printf("(item   ");
}

 /* }}} */
 /* {{{  begin deflist */

{BEGIN_ENV_OPEN}"deflist"{BEGIN_ENV_CLOSE} {
  clean_up_quotes();
  list_environment_flag++; 
  printf("(description  ");
  /* Generate dummy item */
  /* this is to allow \item to be handled cleanly */
  printf("(item   ");
}

 /* }}} */
 /* {{{  begin enumerate */

 /* <(thrown away : 2)>  */
{BEGIN_ENV_OPEN}"enumerate"{BEGIN_ENV_CLOSE} {
  clean_up_quotes();
  list_environment_flag++;
  printf("(enumerate  ");
  /* Generate dummy item */
  /* this is to allow \item to be handled cleanly */
  printf("(item  ");
}

 /* }}} */
 /* {{{ begin itemize */

 /* <(thrown away:4)>  */
{BEGIN_ENV_OPEN}"itemize"{BEGIN_ENV_CLOSE} {
  clean_up_quotes();
  list_environment_flag++; 
  printf("(itemize  ");
  /* Generate dummy item */
  /* this is to allow \item to be handled cleanly */
  printf("(item  ");
}

 /* }}} */
 /* {{{  item */

({WHITESPACE})*{BACKSLASH}"item" {
  clean_up_quotes();
  /* begin a new item */
  /* after ending previous item */
  END_LIST();
  if (!list_environment_flag) {
    fprintf(stderr, "An item was found outside known list environment \n");
    exit(1); 
  }
  /* Note this is a quick fix,
     and will leave a null list as the first item of each enumerated list  */
  printf("(item  ");
}

 /* }}} */
 /* {{{  begin equation */

{BEGIN_ENV_OPEN}"equation"{BEGIN_ENV_CLOSE} {
  clean_up_quotes();
  printf("(equation  ");
  BEGIN MATH;
  display_math_flag = 1; 
}

 /* }}} */
 /* {{{  begin eqnarray */

{BEGIN_ENV_OPEN}"eqnarray*"{BEGIN_ENV_CLOSE} | 
{BEGIN_ENV_OPEN}"eqnarray"{BEGIN_ENV_CLOSE} {
  /* starting an eqnarray */
  array_flag++;
  BEGIN MATH;
  display_math_flag = 1;
  printf("(eqnarray  ");
  /* start the first eqnarray row */
  printf(" ( ");
  /* Start the first eqnarray element */
  printf (" (  ");
}

 /* }}} */
 /* {{{  begin eqalign */

 /* handling like eqnarray for the present */ 
{BEGIN_ENV_OPEN}"eqalign*"{BEGIN_ENV_CLOSE} | 
{BEGIN_ENV_OPEN}"eqalign"{BEGIN_ENV_CLOSE} {
  /* starting an eqalign */
  /* Latex eqaligns are in math mode, so remember this */
display_math_flag =1; 
  BEGIN MATH;
  array_flag++;
  printf("(eqalign  ");
  /* start the first eqalign row */
  printf(" ( ");
  /* Start the first eqalign element */
  printf (" (  ");
}
  

 /* }}} */
 /* {{{ begin tabular  */

{BEGIN_ENV_OPEN}"tabular}"({POS_TAB_ARG})?({OPT_TAB_ARG})?({WHITESPACE})*  {
  clean_up_quotes();
  /* starting a table  */
  table_flag++;
  printf("(tabular  ");
  /* start the first table  row */
  printf(" ( ");
  /* Start the first table  element */
  printf (" (  ");
}

 /* }}} */
   /* {{{ begin cases  */

  /*  cases handled like table environment */
  /* allow for text inside math mode by saving state. If this works, use */
  /* similar approach for mbox hbox etc. */ 

{BEGIN_ENV_OPEN}"cases}"({OPT_TAB_ARG})?({WHITESPACE})*  {
  clean_up_quotes();
  /* First set up state */
  if (display_math_flag) {
    display_math_flag = 0;
    saved_math_state = DISPLAY_MATH;
    BEGIN 0;
  }
  else if (inline_math_flag) {
    saved_math_state = INLINE_MATH;
    inline_math_flag = 0;
    BEGIN 0;
  }
  
  /* starting a table  */
  table_flag++;
  printf("(cases  ");
  /* start the first table  row */
  printf(" ( ");
  /* Start the first table  element */
  printf (" (  ");
}

  /* }}} */
 /* {{{ begin array */

{BEGIN_ENV_OPEN}"array}"({POS_TAB_ARG})?({OPT_TAB_ARG})?({WHITESPACE})*  {
  clean_up_quotes();
  /* starting an array */
 /* Modified: Tue Oct  6 17:06:17 EDT 1992*/
/* Not marking each element to be inline-math*/
  array_flag++;
  printf("(array  ");
  /* start the first array row */
  printf(" ( ");
  /* Start the first array element */
  printf (" (  ");
};

 /* }}} */
 /* {{{  begin slide */

({BLANK_LINE})*{BEGIN_ENV_OPEN}"slide}{}"   |
{BEGIN_ENV_OPEN}"slide"{BEGIN_ENV_CLOSE} {
  clean_up_quotes();
 /*starting a latex slide */
  printf("(slide  ");
};

 /* }}} */
 /* {{{ begin displaymath  */
{BEGIN_ENV_OPEN}"displaymath"{BEGIN_ENV_CLOSE} {
  clean_up_quotes();
  display_math_flag= 1;
  printf("(display-math "); 
  BEGIN MATH;
}

 /* }}} */
 /* {{{  begin unrecognized environment */

{BEGIN_ENV_OPEN}{ALPHABET_STRING}"}"   |
{BEGIN_ENV_OPEN}{ALPHABET_STRING}"}"({POS_TAB_ARG})?({OPT_TAB_ARG})? | 
{BEGIN_ENV_OPEN}([^}])*"}"  {
     char name[MAXLEN];
  /* Modified: Tue Jan  4 08:05:56 EST 1994*/
/* environment names can have more that alphabets */

   clean_up_quotes();
  /* unrecognized environment */
 /* Modified:    Mon May 10 13:32:58 EDT 1993*/
/* Some new environments maybe declared as enumerable by adding their */
 /* name to list_env_names. Handle this by checking if the env name */
 /* present in the table*/

   strcpy(name, latex_env_name(yytext));
   if (list_env_p(name)){
   printf("( new-environment \"%s\" ", name); 
     /* expect items in this env */
     list_environment_flag++; 
  /* Generate dummy item */
  /* this is to allow \item to be handled cleanly */
  printf("(item  ");
   }
   else {
   printf("( new-environment \"%s\" ", name); 
 }
}

 /* }}} */

 /* }}} */
 /* {{{  sectioning constructs */
({WHITESPACE})*{BACKSLASH}"grieschapter" |
({WHITESPACE})*{BACKSLASH}"chapter*" |
({WHITESPACE})*{BACKSLASH}"chapterx" | 
({WHITESPACE})*{BACKSLASH}"chapter" {
  if (chapter_flag ) { 
    if(section_flag){
      /* Already in a section so end it */
      if(subsection_flag){
        /* Also inside a subsection so end it first */
        if (subsubsection_flag) {
          /* end the subsubsection */
          END_LIST();
          subsubsection_flag = 0;
        }
        END_LIST();
        /* end subsection*/
        subsection_flag =0;
      }
      clean_up_quotes();
      /* end section */
      END_LIST();
      section_flag = 0;
    }
    /* new chapter, end previous chapter */
    END_LIST();
    printf("\n(chapter ");}
  else{
    clean_up_quotes();
    /* seeing first chapter */
    chapter_flag =1;
    printf("(chapter ");
  }
}


({WHITESPACE})*{BACKSLASH}"griessection" |
({WHITESPACE})*{BACKSLASH}"section*" |
({WHITESPACE})*{BACKSLASH}"section" {
  if(section_flag){
    /* Already in a section so end it */
    if(subsection_flag){
      /* Also inside a subsection so end it first */
      if (subsubsection_flag) {
	/* end the subsubsection */
	END_LIST();
	subsubsection_flag = 0;
      }
      END_LIST();
      /* end subsection*/
      subsection_flag =0;
    }
    clean_up_quotes();
    /* end section */
    END_LIST();
    /* new section */
    printf("(section  ");
  }
  else{
    clean_up_quotes();
    /* seeing first section */
    section_flag =1;
    printf("(section ");
  }
};
({WHITESPACE})*{BACKSLASH}"subsection*" |
({WHITESPACE})*{BACKSLASH}"subsection" {
  if(subsection_flag){
    /* already in a subsection so end it */
    if (subsubsection_flag) {
      /* first end the subsubsection */
      END_LIST();
      subsubsection_flag = 0;
    }
    clean_up_quotes();
    END_LIST();
    /* new subsection */
    printf("(subsection  ");
  }
  else{
    clean_up_quotes();
    subsection_flag =1;
    printf("(subsection ");
  }
}
({WHITESPACE})*{BACKSLASH}"subsubsection*" |
({WHITESPACE})*{BACKSLASH}"subsubsection" {
  if(subsubsection_flag){
    /* already in a subsubsection so end it */
    END_LIST();
    clean_up_quotes();
    /* new subsubsection */
    printf("(subsubsection  ");
  }
  else{
    clean_up_quotes();
    subsubsection_flag =1;
    printf("(subsubsection ");
  }
}

 /* }}} */
 /* {{{  absolute sectioning constructs */

({WHITESPACE})*{BACKSLASH}"achapter" {
  if (chapter_flag ) { 
    if(section_flag){
      /* Already in a section so end it */
      if(subsection_flag){
        /* Also inside a subsection so end it first */
        if (subsubsection_flag) {
            if( aparagraph_flag) {
          /* end the aparagraph */
          END_LIST();
          aparagraph_flag = 0;
        }
          /* end the subsubsection */
          END_LIST();
          subsubsection_flag = 0;
        }
        END_LIST();
        /* end subsection*/
        subsection_flag =0;
      }
      clean_up_quotes();
      /* end section */
      END_LIST();
      section_flag = 0;
    }
    /* new chapter, end previous chapter */
    END_LIST();
    printf("\n(achapter ");}
  else{
    clean_up_quotes();
    /* seeing first chapter */
    chapter_flag =1;
    printf("(achapter ");
  }
}

({WHITESPACE})*{BACKSLASH}"asection" {
  if(section_flag){
    /* Already in a section so end it */
    if(subsection_flag){
      /* Also inside a subsection so end it first */
      if (subsubsection_flag) {
         if( aparagraph_flag) {
          /* end the aparagraph */
          END_LIST();
          aparagraph_flag = 0;
        } 
	/* end the subsubsection */
	END_LIST();
	subsubsection_flag = 0;
      }
      END_LIST();
      /* end subsection*/
      subsection_flag =0;
    }
    clean_up_quotes();
    /* end section */
    END_LIST();
    /* new section */
    printf("(asection  ");
  }
  else{
    clean_up_quotes();
    /* seeing first section */
    section_flag =1;
    printf("(asection ");
  }
};


({WHITESPACE})*{BACKSLASH}"asubsection" {
  if(subsection_flag){
    /* already in a subsection so end it */
    if (subsubsection_flag) {
        if( aparagraph_flag) {
          /* end the aparagraph */
          END_LIST();
          aparagraph_flag = 0;
        }
      /* first end the subsubsection */
      END_LIST();
      subsubsection_flag = 0;
    }
    clean_up_quotes();
    END_LIST();
    /* new subsection */
    printf("(asubsection  ");
  }
  else{
    clean_up_quotes();
    subsection_flag =1;
    printf("(asubsection ");
  }
}


({WHITESPACE})*{BACKSLASH}"asubsubsection" {
  if(subsubsection_flag){
      if( aparagraph_flag) {
          /* end the aparagraph */
          END_LIST();
          aparagraph_flag = 0;
        }
    /* already in a subsubsection so end it */
    END_LIST();
    clean_up_quotes();
    /* new subsubsection */
    printf("(asubsubsection  ");
  }
  else{
    clean_up_quotes();
    subsubsection_flag =1;
    printf("(asubsubsection ");
  }
}

({WHITESPACE})*{BACKSLASH}"apar" {
  if(aparagraph_flag){
    /* already in a aparagraph  so end it */
      END_LIST();
    clean_up_quotes();
    /* new aparagraph */
    printf("(apar  ");
  }
  else{
    clean_up_quotes();
    aparagraph_flag =1;
    printf("(apar ");
  }
}

 /* }}} */
 /* {{{  end various environments */

 /* {{{  end abstract */

({WHITESPACE})*{BACKSLASH}"end{abstract}"({H_SPACE})*  {
  clean_up_quotes();
if (aparagraph_flag) {
END_LIST();
aparagraph_flag=0;}
  /* end abstract */
  END_LIST();
}

 /* }}} */
 /* {{{ end center */

({WHITESPACE})*{BACKSLASH}"end{center}"({H_SPACE})*  {
  clean_up_quotes();
  /* end center */
  END_LIST();
}

 /* }}} */
 /* {{{ end quote */

({WHITESPACE})*{BACKSLASH}"end{quote}"({H_SPACE})*  {
  clean_up_quotes();
  /* end quote */
  END_LIST();
}

 /* }}} */
 /* {{{  end quotation */

({WHITESPACE})*{BACKSLASH}"end{quotation}"({H_SPACE})*  {
  clean_up_quotes();
  /* end quotation */
  END_LIST();
}

 /* }}} */
 /* {{{  end equation */

({WHITESPACE})*{BACKSLASH}"end{equation}"({H_SPACE})*  {
  clean_up_quotes();
  /* end equation */
  END_LIST();
  BEGIN 0;
  display_math_flag =0; 
};

 /* }}} */
 /* {{{ end eqnarray */

({WHITESPACE})*{BACKSLASH}"end{eqnarray*}"({H_SPACE})*  |
({WHITESPACE})*{BACKSLASH}"end{eqnarray}"({H_SPACE})*  {
  /* close off everything for eqnarray */
  array_flag--;
  /* First close last element */
  END_LIST();
  /* close final row */
  END_LIST();
  /* and finally the eqnarray */
  END_LIST();
  /* Leave math mode */
display_math_flag = 0; 
  BEGIN 0;
};

 /* }}} */
    /* {{{ end eqalign */

({WHITESPACE})*{BACKSLASH}"end{eqalign*}"({H_SPACE})*  |
({WHITESPACE})*{BACKSLASH}"end{eqalign}"({H_SPACE})*  {
  /* close off everything for eqalign */
  array_flag--;
  /* First close last element */
  END_LIST();
  /* close final row */
  END_LIST();
  /* and finally the eqalign */
  END_LIST();
  /* Leave math mode */
display_math_flag = 0; 
  BEGIN 0;
};

  /* }}} */
 /* {{{  end array */

({WHITESPACE})*{BACKSLASH}"end{array}"({H_SPACE})* {
  /* close off everything for array */
  array_flag--;
  /* First close last element */
  END_LIST();
  /* close final row */
  END_LIST();
  /* and finally the array */
  END_LIST();
}

 /* }}} */
 /* {{{  end tabular */

({WHITESPACE})*{BACKSLASH}"end{tabular}"({H_SPACE})* {
  /* close off everything for table */
  table_flag--;
  /* First close last element */
  END_LIST();
  /* close final row */
  END_LIST();
  /* and finally the table  */
  END_LIST();
}

 /* }}} */
     /* {{{  end cases */

 /* Cases handled like tabular */ 

({WHITESPACE})*{BACKSLASH}"end{cases}"({H_SPACE})* {
  /* close off everything for table */
  table_flag--;
  /* First close last element */
  END_LIST();
  /* close final row */
  END_LIST();
  /* and finally the table  */
  END_LIST();
  /* reset state to math if necessary */
  if (saved_math_state) {
    switch (saved_math_state) {
    case INLINE_MATH: 
        inline_math_flag = 1;
      saved_math_state = 0;
      BEGIN MATH;
      break;
      case DISPLAY_MATH: 
        display_math_flag = 1;
      BEGIN MATH;
      saved_math_state = 0;
      break;
    default:
      fprintf(stderr, "Unknown saved_math_state %d \n", saved_math_state);
        exit(1); 
    }
  }
}

  /* }}} */
 /* {{{ end enumerate */

 /* <(thrown away)>  */
({WHITESPACE})*{BACKSLASH}"end{enumerate}"({H_SPACE})* {
  clean_up_quotes();
  list_environment_flag--; 
  /* end enumerated list */
  /* First end last item */
  END_LIST();
  /* and now end the enumerated list itself */
  END_LIST();
}

 /* }}} */
 /* {{{  end description */

({WHITESPACE})*{BACKSLASH}"end{description}"({H_SPACE})* {
  clean_up_quotes();
  list_environment_flag--;
  /* end descriptiond list */
  /* First end last item */
  END_LIST();
  /* and now end the description  list itself */
  END_LIST();
}

 /* }}} */
 /* {{{  end itemize */

 /* <(thrown away :3)>  */

({WHITESPACE})*{BACKSLASH}"end{itemize}"({H_SPACE})* {
  clean_up_quotes();
  list_environment_flag--; 
  /* end itemized list */
  /* First end last item */
  END_LIST();
  /* and now end the itemized list itself */
  END_LIST();
}

 /* }}} */
 /* {{{  end document */

({WHITESPACE})*{BACKSLASH}"end{document}" {
  clean_up_quotes();
  /* Close everything which was opened. */
  if (aparagraph_flag) {
    END_LIST();
  }
    if (subsubsection_flag) {
    END_LIST();
  }
  if(subsection_flag){
    END_LIST();
  }
    if(section_flag){
    END_LIST();
  }
  if (chapter_flag) {
    END_LIST();
  }
  /* and finally end the document */
  END_LIST();
}

 /* }}} */
 /* {{{  end slide */

({WHITESPACE})*{BACKSLASH}"end{slide}"({BLANK_LINE})*  {
  clean_up_quotes();
 /*end the slide */
  END_LIST();
}

 /* }}} */
 /* {{{ end displaymath  */

({WHITESPACE})*{BACKSLASH}"end{displaymath}"  {
  display_math_flag = 0;
  END_LIST();
  BEGIN 0;
}
  

 /* }}} */
 /* {{{  end unrecognized environment */

{BACKSLASH}"end{"{ALPHABET_STRING}("*")?"}" |
{BACKSLASH}"end{"([^}])*("*")?"}"  {
  /* unknown environment ending */
 /* Modified:   Mon May 10 16:27:35 EDT 1993*/
/* handle new environments that can be lists */ 
       char name[MAXLEN];
  clean_up_quotes();
   strcpy(name, latex_env_name(yytext));
   if (list_env_p(name)){
     /* this is a list environment */ 
  list_environment_flag--; 
  /* First end last item */
  END_LIST();
  /* and now end the  environment  list itself */
  END_LIST();
   }
       else{ 
  printf(")");
}
}

 /* }}} */

 /* }}} */
 /* {{{  tex control sequences eg macro names */

{BACKSLASH}([^a-zA-Z]) {
  /* handle single characters with a backslash in front eg \. etc. */
  printf(" \"%s\" ",  escape_quotes(yytext + 1)); 
}

<MATH>{BACKSLASH}({ALPHABET})*   {  
  printf("(math-cs  ");
  /* convert macro names to strings
     since lisp is case insensitive */
  printf(" \"%s\" ", (yytext + 1));
  /* after skipping the initial backslash */
  END_LIST();
};

{BACKSLASH}({ALPHABET})*   { 
  printf("(cs  ");
  /* convert macro names to strings
     since lisp is case insensitive */
  printf(" \"%s\" ",escape_quotes( (yytext + 1)));
  /* after skipping the initial backslash */
  END_LIST();
};

 /* }}} */
 /* {{{  be smart about numbers */

<MATH>{MATH_NUMBER} {
 /* numbers in math mode,
 catch them first */
  printf("(math-number  \"%s\")  ", yytext);
}
{TIME_NUMBER} {
  /* Dectalk speaks time numbers correctly */
PRINT_AS_STRING(yytext); 
}
{TEXT_NUMBER} {
  /* in math mode and text mode recognize numbers and do not break them up */
  printf("(text-number \"%s\") ",yytext);
}

 /* }}} */
 /* {{{  words handled according to mode */

<MATH>"''"{H_SPACE}* {
  printf("\"double-prime\"");
}
{ALPHABET} |
{ALPHABET}({LETTER})*("'")?({ALPHABET})+    {
  if (IS_MATH()) {
    /* In math mode, the string should be broken up
       into strings of one character ie "a+b" is "a" "+" "b"
       since TeX allows for only plain single letter variables */
    int i ;
    for(i = 0; i< yyleng; i++)
      printf(" \"%c\" ", (yytext[i]));
  }
  else {
    /* convert text to strings */
    /* not escape quotes since umlaut now handled as a letter*/
    /* if this causes trouble, reintroduce escape_quote as in */
/*     printf(" \"%s\"", escape_quotes(yytext));*/
        printf(" \"%s\"", yytext);
  }
};

 /* }}} */
              /* {{{  quotes */

{H_SPACE}*"\"" {
  if (!inline_quote_flag){
  /* inline quotation */
  inline_quote_flag = 1;
  printf("(inline-quote ");}
  else {
    REJECT;
  }
};


{H_SPACE}*"``" {
  if (!inline_quote_flag){
  /* inline quotation */
  inline_quote_flag = 1;
  printf("(inline-quote ");}
};

"\""{H_SPACE}* |
"''"{H_SPACE}*  {
  /* matching " here is a concession */
  if (inline_quote_flag) {
    printf(" \"''\" ");
    /* Marking matched inline-quote */
    printf(")");
    inline_quote_flag  = 0;
  }
  else {
    /* This does not match a quotation, so just put it in the text: */
    printf("\"%s\" ",escape_quotes(yytext));
  }
};

 /* }}} */
 /* {{{  punctuations */

{PUNCT} {
printf("\"%s\"", escape_quotes(yytext));
};

 /* }}} */
 /* {{{  skip multiple white space */

[\t \ ]* ECHO;

 /* }}} */
 /* {{{ Trap things that are not caught and echo to stderr */

 /* form feed and control m */
 /* control m shows up from outline mode in emacs */ 
{CONTROL-M} ;
{CONTROL-L} ;
"#" printf("\"#\" "); 
.  fprintf(stderr, "\n this escaped %s \n", yytext);

 /* }}} */
 /* {{{ section 3: function definitions */

%%
/* {{{ latex_env_name */

char *  latex_env_name(s)
     char * s;
{
  /* return string containing the contents of      {     foo     }      */
  static char result[MAXLEN];
  
  char * head;
  int i =0;
  head = strchr(s,'{');
  head++;
while ( *head && (*head !='}') & (i < MAXLEN - 1)) {
result[i] = *head;
i++;
  head++;
}
result[i] ='\0';
  return(result);
}

/* }}} */
/* {{{ escape_quotes */

char * escape_quotes(s)
     char *s; 
{
static   char t[2*MAXLEN];
  int i ,j;

  for(i=0, j=0; i <= strlen(s); i++, j++){
if ( s[i] == '"') 
      t[j++]='\\';
t[j] = s[i];
}
return(t); 
}

/* }}} */
/* {{{ Clean_up_quotes */

int clean_up_quotes()

{
  /* Catch run away quotes. */
  /* Just close the list, process-inline-quote
     can now catch this by the fact that there is no matching '' at the end.
     Uses the global variable inline_quote_flag      */
  if (inline_quote_flag) {
    printf(")");
    inline_quote_flag = 0;
  }
}

/* }}} */
/* {{{ get_macro_name */

char * get_macro_name(s)
     char * s;
{
  static char result[MAXLEN];
  char * head;
  int i =0;
  head = strrchr(s, '\\');
  head++;
  while ( *head && (i < (MAXLEN - 1))) {
result[i] = *head;
i++;
  head++;
}
result[i] ='\0';
  return(result);
}

/* }}} */
/* {{{ get_tex_filename */

char *  get_tex_filename(result,s)
     char * result; 
     char * s;
{
  /* return string containing the contents of      {     foo     } */
  /*use env variable READ-TEX-DIR */
  char * filename; 
char * dirname; 
  int  len;
dirname = getenv("READ-TEX-DIR");
  filename = (char *)malloc(MAXLEN); 
  len = strlen(s);
  strncpy (filename, s+1, len-2);
  if (dirname != NULL){
    strcpy(result, dirname);
    strcat(result, filename);
  }
  else {
    strcpy(result, filename);
  }
if (strstr(result, ".") == NULL) strcat(result, ".tex");
  return(result);
}

/* }}} */

 /* {{{ get_tex_filename_without_braces */

char *  get_tex_filename_without_braces(result,s)
     char * result; 
     char * s;
{
  /* return string containing the contents of      foo      */
  /*use env variable READ-TEX-DIR */
  char * filename; 
char * dirname; 
  int  len;
dirname = getenv("READ-TEX-DIR");
  filename = (char *)malloc(MAXLEN); 
  len = strlen(s);
  strcpy (filename, s);
  if (dirname != NULL){
    strcpy(result, dirname);
    strcat(result, filename);
  }
  else {
    strcpy(result, filename);
  }
if (strstr(result, ".") == NULL) strcat(result, ".tex");
  return(result);
}
  

 /* }}} */
/* {{{ list_env_p */

#define MAX_BUFF 80 

static char *  list_env_names[MAX_BUFF] = {
"list",
"exercises",
"Exercises",
"closeitemize", 
"alphlist",
"Alphlist",
"chapterex"
}; 

int  list_env_p(env_name)
char * env_name; 
{
char *ptr ;
int i=0; 
ptr =  list_env_names[i]; 
while  ( (ptr != NULL) )
{
  if (strcmp(ptr, env_name)== 0)
    return(1); /*found it */ 
  ptr = list_env_names[++i];  /*continue looking*/ 
}
return(0);  /* not found*/ 
}

/* }}} */

 /* }}} */
 /* {{{ emacs local variables; */

/* local variables: */
/* folded-file: t */
/* end: */

 /* }}} */ 
