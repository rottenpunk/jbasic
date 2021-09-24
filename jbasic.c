//--`---------------------------------------------------------------------------
//
//         jbasic -- John's personal basic interpreter
//
//         copyright (c) 1999-2003 by John C. Overton
//         All rights reserved.  Generally, with personal
//         approval of the auther, you can use this program
//         as long as this copyright header remains in
//         the program source header.
//
//-----------------------------------------------------------------------------

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <ctype.h>
//#include <conio.h>

#ifdef MQX
#include <mqx.h>
//#include <mqx_prv.h>
#include <bsp.h>
#include <fio.h>

//char*  kernel_data_ptr;

#define malloc(i) _mem_alloc(i)
#define free(p)   _mem_free(p)
#endif


typedef unsigned long U32;
typedef          long I32;
typedef unsigned short U16;
typedef          short I16;
typedef unsigned char BYTE;
typedef          int  BOOL;
#define TRUE  1
#define FALSE 0

//-----------------------------------------------------------------------------
//  Help text... (Will display if you use the help command...
//-----------------------------------------------------------------------------
char  helpCopyright[] = "\nJBASIC -- Copyright (c) 1999-2003 by John C. Overton";
char*  helpText[] = {
    "JBASIC is a small integer BASIC interpreter. Something that I did in\n",
    "a few hours of free time.\n\n",
    "Enter lines of BASIC statements by preceeding statement with line number.\n",
    "Or enter statement in directly to execute immediately.\n\n",
    "Valid Statements:\n\t",
    "rem    - Remark. Characters following rem are not executed.\n\t",
    "print  - Print strings (inside with double quotes) interspersed with\n\t",
    "         arithmetic expressions.\n\t",
    "printf - A c style printf() routine. An alternative to print\n\t",
    "goto   - Start executing from line number n.\n\t",
    "gosub  - Start executing a subroutine from line number n.\n\t",
    "return - Return from subroutine call (from gosub).\n\t",
    "if     - Test expression and if true, use then to go to line n.\n\t",
    "then   - Used with if to give line number n. Then must be on same\n\t",
    "         line as if statement.\n\t",
    "else   - Used with if to give line number n when expression is false.\n\t",
    "for    - The usual basic \"for\" statement, for example: for I = 1 to 10 by 2.\n\t",
    "run    - Run program from start or from line number n.\n\t",
    "list   - List entire program, or from line number n.\n\t",
    "stop   - Stop program execution (not very useful when immediately\n\t",
    "         executing).\n\t",
    "new    - Delete previous program and start a new one.\n\t",
    "clear  - Clear all variables to 0.\n\t",
    "bye    - End JBASIC.\n\t",
    "step   - Single step through program.\n\t",
    "cont   - Continue program from last stop command.\n\t",
    "end    - End program execution.\n\t",
    "tron   - Turn on program tracing.\n\t",
    "troff  - Turn off program tracing.\n\t",
    "load   - Load a basic program from disk.\n\t",
    "save   - Save a basic program to disk.\n\t",
    "help   - Print help screen (this output that you are looking at now).\n",
    NULL
};




//-----------------------------------------------------------------------------
//  Initial program...
//-----------------------------------------------------------------------------

char initialProgram[] = {
//    "010 var1 = peek(10) + 2\n"
//    "020 print \"var1 = \", var1\n"
//    "030 var2 = poke(0x10, 0x20)\n"
//    "040 print \"var2 = \", var2\n"

      "100 var2 = 0\n"
      "110 var1 = 10000\n"
      "120 var1 = var1 - 1\n"
      "130 if var1 == 0 then 150\n"
      "140 goto 120\n"
      "150 print \"var2 = \", var2\n"
      "160 var2 = var2 + 1\n"
      "170 goto 110\n"

//    "010 tron\n"
//    "100 for var1 = -5 to 5 by 2\n"
//    "110 for var2 = 0 to -10 by -3\n"
//    "120 print \"var1 = %-08d, var2 = %-8d*\\n\", var1, var2\n"
//    "130 next var2\n"
//    "140 next var1\n"

};


//-----------------------------------------------------------------------------
//  Define Identifiers.  These identifiers are returned from Lex() and also
//  put into Token by Lex()...
//-----------------------------------------------------------------------------
#define ERROR           128
#define EOL             129
#define ID              130             // Identifier.
#define ICON            131             // Integer value.  Integer is in Value.
#define STRING          132
#define FUNCTION        133

#define KEYWORD_START   196             // The following IDs are keywords...
#define BY              196
#define BYE             197
#define CLEAR           198
#define CONT            199
#define ELSE            200
#define END             201
#define FOR             202
#define GOTO            203
#define GOSUB           204
#define HELP            205
#define IF              206
#define LIST            207
#define LOAD            208
#define NEW             209
#define NEXT            210
#define PRINT           211
#define PRINTF          212
#define REM             213
#define RETURN          214
#define RUN             215
#define QUIT            216
#define SAVE            217
#define STEP            218
#define STOP            219
#define THEN            220
#define TO              221
#define TROFF           222
#define TRON            223



//-----------------------------------------------------------------------------
//  Function table provides list of callable functions...
//-----------------------------------------------------------------------------
typedef struct  func    {
    char*          func_name;
    I32            (*func_addr)();
    BYTE           func_params;
} func;

//-----------------------------------------------------------------------------
//  Storage of Internal Lines
//-----------------------------------------------------------------------------
typedef struct  line    {
    int            line_num;
    char*          line_text;
    struct  line*  next;
} line;

//-----------------------------------------------------------------------------
//  Storage of Symbol Table
//-----------------------------------------------------------------------------
typedef struct  sym     {
    char*          sym_name;
    I32            sym_val;
    struct  sym*   next;
} sym;

#define HASH_SIZE   32

#define STACK_SIZE  16



//-----------------------------------------------------------------------------
//  Structure to keep track of who is logged on...
//-----------------------------------------------------------------------------
typedef struct name {
    struct name* Next;
    struct name* Prev;
    char* Name;
} NAME;

NAME* head = NULL;            // Global anchor of all users logged on.
BOOL  FlagDebug = FALSE;      // Debug flag for printing out debug info.
char  InitialCommand[128];
BOOL  FlagInitialCommand = FALSE; // Initial command on command line.
char  InitialFilename[128];
BOOL  FlagInitialFilename = FALSE;
BOOL  FlagNoHeading = FALSE;


//-----------------------------------------------------------------------------
//  Global data structure definition...
//-----------------------------------------------------------------------------

typedef struct global {

    sym*        ht[HASH_SIZE];              // Symbol hash table.
    char        currentLine[256];           // Current Expression
    char        tokenText[256];             // Text of Current Token
    char*       linePtr;                    // Current Character
    int         Token;                      // Current Token
    I32         Value;                      // Numerical Value of Current Token
    func*       functionID;                 // Current Token's Function Record
    line*       head;                       // Chain of source lines.
    line**      SP;                         // Current stack entry.
    line*       stack[STACK_SIZE];          // Subr stack. Stack of ptr to lines.
    unsigned    PC;                         // Current Line Number
    line*       pPC;                        // Pointer to Current Line
    char        jump_flag;                  // Set if Instruct Changes Prog Flow
    char        trace_flag;                 // Set if Trace Flag is On
    char        exit_flag;                  // Exit out of jbasic.
    jmp_buf     error;                      // A jumb buffer to lead us to error routine.
    NAME*       name;                       // Info about logged on user.

} global;


//-----------------------------------------------------------------------------
//   Table of callable functions...
//-----------------------------------------------------------------------------

I32 peek(  global* g, I32 i );
I32 peek2( global* g, I32 i );
I32 peek1( global* g, I32 i );
I32 poke(  global* g, I32 i, I32 x);
I32 poke2( global* g, I32 i, I32 x);
I32 poke1( global* g, I32 i, I32 x);
I32 users( global* g );
I32 geti ( global* g );

func     function[] = {
                        "peek",     peek,         1,
                        "peek2",    peek2,        1,
                        "peek1",    peek1,        1,
                        "poke",     poke,         2,
                        "poke2",    poke2,        2,
                        "poke1",    poke1,        2,
                        "geti",     geti,         0,
                        "users",    users,        0,

                         // NULL  // Temp entry until some are added.
                      };


//-----------------------------------------------------------------------------
//  BASIC Keywords. The order matches the identifiers above...
//-----------------------------------------------------------------------------
char    *keyword[] = {
    "by",
    "bye",
    "clear",
    "cont",
    "else",
    "end",
    "for",
    "goto",
    "gosub",
    "help",
    "if",
    "list",
    "load",
    "new",
    "next",
    "print",
    "printf",
    "rem",
    "return",
    "run",
    "quit",
    "save",
    "step",
    "stop",
    "then",
    "to",
    "troff",
    "tron"
};


//-----------------------------------------------------------------------------
//  Error message codes and error messages...
//-----------------------------------------------------------------------------
#define E_NOLPAREN      1
#define E_BADTOK        2
#define E_SYNTAX        3
#define E_DIV0          4
#define E_BADLINE       5
#define E_BADSTRING     6
#define E_NOMEM         7
#define E_NOEQUAL       8
#define E_NOTHEN        9
#define E_NOELSE        10
#define E_STKFLOW       11
#define E_BADEOL        12
#define E_BADRET        13
#define E_PARAMS        14
#define E_NORPAREN      15
#define E_BADFORVAR     16
#define E_BADFOREXPR    17
#define E_MISSINGNEXT   18
#define E_INVALIDOP     19
#define E_INVALIDPARMS  20
#define E_BADFILENAME   21
#define E_CANTOPENFILE  22


char    *errorMessages[] = {
    "Missing '('",
    "Bad Token",
    "Syntax Error",
    "Divide by 0",
    "Bad Line Number",
    "Bad String",
    "Out of Memory",
    "Missing '='",
    "Missing THEN Statement",
    "Missing ELSE Statement",
    "Run Time Stack Overflow",
    "Garbage at End of Line",
    "Return w/o Matching GOSUB",
    "Too Many Parameters",
    "Missing ')'",
    "Bad FOR control variable",
    "Bad FOR expression",
    "Missing NEXT",
    "Invalid operator",
    "Invalid parameters",
    "Bad file name",
    "Can't open file"
};



//-----------------------------------------------------------------------------
// Local internal functions...
//-----------------------------------------------------------------------------
line* gotoNextLine( global* g  );
void  login(        global* g );
void  logout(       global* g );
void  listUsers(    global* g );
void  doError(      global* g, unsigned int Code );
void  doLine(       global* g, int   trace);
void  Line(         global* g );
void  Load(         global* g );
void  Save(         global* g );
void  Clear(        global* g );
void  New(          global* g );
void  Command(      global* g );
void  printHelp(    global* g );
I32   Function (    global* g );
void  PrintList (   global* g );
void  Printf(       global* g );
void  Done(         global* g );
void  If(           global* g );
void  Assign(       global* g );
void  List(         global* g );
void  forNext(      global* g );
I32   Expr(         global* g );
I32   XorExpr(      global* g );
I32   AndExpr(      global* g );
I32   NotNegExpr(   global* g );
I32   CompExpr(     global* g );
I32   AddExpr(      global* g );
I32   MultExpr(     global* g );
I32   PrimExpr(     global* g );
void  loadFile(     global* g, char* filename);
void  storeLine (   global* g, int   num,  char*   text );
void  deleteLine (  global* g, int   num );
int   findLine(     global* g, int   num,  line**  hp,  line**  tp );
sym*  findVar(      global* g, char* name );
void  storeVar(     global* g, char* name, int     val );
int   readVar(      global* g, char* name );
void* xalloc(       global* g, int   size );
char* salloc(       global* g, char* s );
int   Lex (         global* g );
void  copyInitialProgram( global* g );
I32   cnvToHex(     char* start, char** end);
I32   cnvToLong(    char* start, char** end);
int   formatNumber( unsigned long  i, BYTE flags, int Prec, int Width, int Base);


//-----------------------------------------------------------------------------
//  Program Starts Here
//-----------------------------------------------------------------------------

void  main(
    int         ArgC,                   // command line argument count.
    char       **ArgV )                 // command line argument ptrs.
{
    char      *Arg;                     // Ptr into command line args.
    global*    g = malloc( sizeof(global));

    *g->currentLine = '\0';             // Indicate no initial file.

    //-------------------------------------------------------------------------
    //  Parse command line arguments...
    //-------------------------------------------------------------------------
    while( --ArgC != 0 ) {              // Go thru all args (decr cnt).
        ++ArgV;                         // Pnt to first/next arg.
        if( *(Arg = *ArgV) == '-' ||    // Do we have an option?
                    *Arg == '/') {      // Either UNIX style or MSDOS?
            switch(*++Arg) {            // test flag...
                case 'd':               // Debug option.
                    FlagDebug = TRUE;
                    break;
				case 'n':
					FlagNoHeading = TRUE;
					break;
                case 'r':               // execute initial command?
                    if (ArgC-- >= 2) {
                        FlagInitialCommand = TRUE;
                        strcpy(InitialCommand,  *(++ArgV));						
                    }
                    break;
                default:
                    fprintf(stderr,
                           "Invalid Flag specified on command line: %s\n",
                           *ArgV);
                    exit(1);
            }
        } else {
            strcpy(InitialFilename, Arg); // Save file to execute.
            FlagInitialFilename = TRUE;
            if (!FlagInitialCommand) {
                FlagInitialCommand = TRUE;
                strcpy(InitialCommand, "run");
            }
        }
    }

    g->head       = NULL;              // Initialize Internal Spaces.
    g->PC         = 0;
    g->pPC        = NULL;
    g->SP         = &g->stack[0];
    g->trace_flag = 0;
    g->exit_flag  = 0;

    memset( (void *)g->ht, 0, sizeof(g->ht) );

	if (!FlagNoHeading)
		printf("%s\n\n", helpCopyright);

    // login(g);       // Make user log in.

    if (initialProgram && !FlagInitialFilename)
        copyInitialProgram( g );

    if (FlagInitialFilename) 
        loadFile(g, InitialFilename);

    while (!g->exit_flag) {
        if (!FlagInitialCommand) {
            printf( "> " );                // Prompt
            if(feof(stdout))
               break;
            gets( g->currentLine );        // Get Line
        }  else {
            FlagInitialCommand = FALSE;
			strcpy(g->currentLine, InitialCommand);
			g->exit_flag = TRUE;
		}
        g->linePtr = g->currentLine;
        g->tokenText[0] = 0;
        if (! setjmp(g->error)) {      // Set Error Vector
            Lex ( g );                 // Prime Parser
            Line( g );                 // Evaluate Line
        }
    }

	if (!FlagNoHeading)	
		printf( "Thanks for using JBASIC!  Good Bye...\n" );

    // logout( g );
    free( g );                          // Free global structure.
}



//-----------------------------------------------------------------------------
// doError() -- Print an error message...
//-----------------------------------------------------------------------------
void doError( global* g, unsigned Code )
{
    int     col;

        col = g->linePtr - g->currentLine;
        printf( "%s\n", g->currentLine );
        while (col--)
            putchar ( ' ' );
        putchar( '^' ); putchar ( '\n' );
        printf( "%s", errorMessages[ Code - 1] );
        if (g->pPC)
            printf( " in Line %d\n", g->PC );
        else
            putchar( '\n' );
        longjmp ( g->error, Code );
}



//-----------------------------------------------------------------------------
// gotoNextLine() -- Proceed to next line...
//-----------------------------------------------------------------------------
line* gotoNextLine( global* g )
{
    g->pPC = g->pPC->next;
    if (g->pPC == NULL)
        return NULL;
    g->PC = g->pPC->line_num;
    strcpy( g->currentLine, g->pPC->line_text );
    g->linePtr = g->currentLine;
    return g->pPC;
}



//-----------------------------------------------------------------------------
// doLine() --  Execute a Line...
//-----------------------------------------------------------------------------
void doLine( global* g,  int  trace)
{

    if (! g->pPC)
        Done( g );
    if ( trace )
        printf( "[%d] %s", g->PC, g->pPC->line_text );
    strcpy( g->currentLine, g->pPC->line_text );
    g->linePtr = g->currentLine;
    Lex( g );
    g->jump_flag = 0;
    Command( g );
    if (! g->jump_flag)
        gotoNextLine( g );
}



//-----------------------------------------------------------------------------
// Line() --  Decide whether to Modify Program or Execute from Prompt...
//-----------------------------------------------------------------------------
void Line( global* g )
{
    char    *p;

    if (g->Token == EOL)
        return;
    if (g->Token == ICON) {
        p = g->linePtr;
        Lex( g );
        if (g->Token != EOL)
            storeLine(g, g->Value, p );
        else
            deleteLine( g, g->Value );
    } else {
        Command( g );
    }
}



//-----------------------------------------------------------------------------
//  Command() -- Immediate execution of a BASIC statement...
//-----------------------------------------------------------------------------
void Command( global* g )
{
    line*   q;

    switch ( g->Token ) {

        case QUIT:
        case BYE:
            g->exit_flag = 1;
            longjmp ( g->error, 1 );
            break;

        case CLEAR:
            Clear( g );
            return;

        case CONT:
            Lex( g );
            if (g->Token == ICON) {
                if ( !findLine( g, g->Value, &g->pPC, &q ))
                    doError ( g, E_BADLINE );
            } else {
                g->pPC = g->pPC->next;
            }

            if (g->pPC)
                g->PC = g->pPC->line_num;

            goto do_run;

        case END:
            Done( g );

        case FOR:
            forNext( g );
            return;

        case FUNCTION:
            Function( g );
            return;

        case GOSUB:
            Lex( g );
            if ( (line**)g->SP > &(g->stack[STACK_SIZE-1]))
                doError( g, E_STKFLOW );
            *g->SP++ = g->pPC->next;
            if (g->Token != ICON || !findLine( g, g->Value, &g->pPC, &q ))
                doError( g, E_BADLINE );
            g->PC = g->Value;
            g->jump_flag = 1;
            return;

        case GOTO:
            Lex( g );
            if (g->Token != ICON || !findLine( g, g->Value, &g->pPC, &q ))
                doError( g, E_BADLINE );
            g->PC = g->Value;
            g->jump_flag = 1;
            return;

        case HELP:
            printHelp( g );
            return;

        case ICON:
        case ID:
            Assign( g );
            return;

        case IF:
            If( g );
            return;

        case LIST:
            List( g );
            return;

        case LOAD:
            New( g );
            Clear( g );
            Load( g );
            return;

        case NEW:
            New( g );
            Clear( g );
            return;

        case PRINT:
            PrintList( g );
            return;

        case PRINTF:
            Printf( g );
            return;

        case REM:
            return;

        case RETURN:
            Lex( g );
            if (g->Token != EOL)
                doError( g, E_BADEOL );
            if ((line **)g->SP <= &(g->stack[0]))
                doError( g, E_BADRET );
            g->pPC = *--g->SP;
            g->PC = g->pPC->line_num;
            g->jump_flag = 1;
            return;

        case RUN:
            Lex( g );
            if (g->Token == ICON)
                g->PC = g->Value;
            else
                g->PC = 0;
            if ( !findLine( g, g->PC, &g->pPC, &q )) {
                if (g->PC)
                    doError( g, E_BADLINE );
                g->PC = g->pPC->line_num;
            }
            g->SP = &g->stack[0];
do_run:
            g->jump_flag = 1;
            while (1) {
                doLine ( g, g->trace_flag );        // Flag Parameter for Trace
#ifdef MQX
                _time_delay(0);                     // Let others run.
#endif
                if(feof(stdout))
                    return;
//                if (kbhit() && getchar() == 0x03)
//                    break;
            }

        case SAVE:
            Save( g );
            return;

        case STEP:
            Lex( g );
            g->PC = 0;
            if (g->Token == ICON) {
                if ( !findLine( g, g->Value, &g->pPC, &q ))
                    doError( g, E_BADLINE );
                g->PC = g->pPC->line_num;
            }
            doLine ( g, 1 );
            return;

        case STOP:
            printf( "Break at %d\n", g->PC );
            longjmp( g->error, 1 );

        case TROFF:
            g->trace_flag = 0;
            return;

        case TRON:
            g->trace_flag = 1;
            return;

    }

    doError( g, E_SYNTAX );
}




//-----------------------------------------------------------------------------
// printHelp() -- Print a screen containing some help information...
//-----------------------------------------------------------------------------
void printHelp ( global* g )
{
    int i = 0;

    printf("\n%s\n\n", helpCopyright);

    while(helpText[i] != NULL) {
        printf(helpText[i]);
        if (((i+1) % 18) == 0) {
            printf("\nPress ENTER to continue");
            gets( g->currentLine );        // Get Line
        }
        i++;
    }
    printf("\n");
}




//-----------------------------------------------------------------------------
// Clear() -- Clear all variables from symbol table...
//-----------------------------------------------------------------------------
void Clear ( global* g )
{
    int  i;
    sym* s;

    for ( i=0; i < HASH_SIZE; i++)
        while (g->ht[i]) {
            s = g->ht[i]->next;
            free( g->ht[i]->sym_name );
            free( g->ht[i] );
            g->ht[i] = s;
        }
    return;
}




//-----------------------------------------------------------------------------
// New() -- Remove all lines of current program...
//-----------------------------------------------------------------------------
void New ( global* g )
{
    line*  q;

    g->trace_flag = 0;
    while (g->head) {
        q = g->head->next;
        free( g->head->line_text );
        free( g->head );
        g->head = q;
    }

    return;
}





//-----------------------------------------------------------------------------
// Function() -- Call a function. Lookup function name in function table,
//               then call function...
//-----------------------------------------------------------------------------
I32  Function ( global* g )
{
    func    *f;
    int      i;
    I32      rc;
    I32      p[4];

     f = g->functionID;
     Lex( g );
     if (f->func_params) {
         if (g->Token != '(')
             doError( g, E_NOLPAREN );

         Lex( g );

         i = 0;

         while (1) {
             p[i++] = Expr( g );
             if (g->Token == ',') {
                 Lex( g );
                 if (i < sizeof(p) / sizeof(p[0]))
                     continue;
             }
             break;
         }

         if (i >= sizeof(p) / sizeof(p[0]) || i != f->func_params)
             doError( g, E_PARAMS );
         if (g->Token != ')')
             doError( g, E_NORPAREN );
         Lex( g );
         rc = (*(f->func_addr))(g, p[0], p[1], p[2], p[3]);
     } else {
         rc = (*(f->func_addr))(g);
     }

     return rc;
}



//-----------------------------------------------------------------------------
// PrintList() --  Outputs Print Parameter to stdout...
//-----------------------------------------------------------------------------
void PrintList ( global* g )
{

    while (1) {

        Lex( g );

        if (g->Token != ID &&
            g->Token != ICON &&
            g->Token != '(' &&
            g->Token != STRING)
            break;

        if (g->Token == STRING) {
            printf("%s", g->tokenText );
            Lex(g);
        } else {
            printf( "%d ", Expr(g) );
        }

        if (g->Token != ',')
            break;
    }
    putchar( '\n' );
}



//-----------------------------------------------------------------------------
// Printf() --  Outputs Print Parameter to stdout using printf style formatting...
//-----------------------------------------------------------------------------
void Printf ( global* g )
{
    char  savedFormat[256];
    char* p;
    char* s;
    int   rc = 0;
    int   Base;
    int   Width;
    int   Prec;
    BYTE  flags = 0;
#define P_LEFT     0x01
#define P_FILL     0x02
#define P_SIGN     0x04
#define P_UPCASE   0x08
#define P_POINTER  0x10
    I32   value;

    Lex( g );

    if (g->Token != STRING)                   // First parameter must be format string.
        doError( g, E_BADSTRING );          // Otherwise error.

    // Save the formatting string...
    strncpy(savedFormat, g->tokenText, sizeof(savedFormat));
    p = savedFormat;

    Lex( g );

    while (*p != '\0') {                   // Loop until we get to end of format string.

        flags = 0;                         // Clear flags.

        if (*p == '\\') {

            switch (*++p) {
                case 'n':
                    putchar('\n');
                    break;
                default:
                    putchar(*++p);
                    break;
            }
            p++;
            continue;
        }

        if (*p != '%') {

            putchar(*p);
            p++;
            continue;

        }

        Base   = 10;
        Width  = 0;
        p++;                               // Bump past %

        if (*p == '%') {                   // If just %% then...
              putchar(*p);                 // just output single %.
              continue;
        }

        if (*p == '-') {                   // Left justify?
            p++;
            flags |= P_LEFT;
        }

        if ( *p == '0' )                   // Width start with 0?
            flags |= P_FILL;               // Then we may have to pad.

        if ( isdigit(*p) )                 // Is width given?
           Width = cnvToLong ( p, &p );    // Yes, format from string.

        if (*p == '.')                     // Is there a precision?
           Prec = cnvToLong ( ++p, &p );   // Yes, format from string.
        else
           Prec = (flags & P_FILL) ? Width : 0; // Otherwise calc default prec.

        if (*p == 'l')                     // Long parameter?
           p++;                            // Just ignore.

        if (g->Token != ',')                  // Commas should separate parameters.
            doError(g, E_INVALIDPARMS);

        Lex( g );

        switch(*p++) {

            case 's':                      // Format a string.
                if (g->Token != STRING)
                    doError( g, E_BADTOK );

                if (Width != 0) {
                    Prec = Width - strlen(g->tokenText);
                    if ( !(flags & P_LEFT))  // Left justify?
                       while(Prec-- > 0)
                           putchar(' ');   // Pad before start of string.
                }
                Width = strlen(g->tokenText);
                s = g->tokenText;
                while(Width--)
                    putchar(*s++);
                continue;

            case 'c':                      // Format a character.
                continue;

            case 'd':                      // Format an integer.
            case 'D':
                flags |= P_SIGN;
              break;

            case 'o':                     // Octal number.
            case 'O':
              Base = 8;
              break;

            case 'X':                      // Hex number uppercase.
                flags |= P_UPCASE;
            case 'x':                      // Hex number lower case.
                Base = 16;
                break;

            case 'P':                      // Far pointer uppercase.
                flags |= P_UPCASE;
            case 'p':                      // Far Pointer lowercase.
                Base = 16;
                flags |= P_POINTER;        // Say it's a pointer.
                Width = 8;                 // Force width to 8 positions.
                break;

            case 'u':                      // Unsigned int.
                break;

            default:                       // Not sure what it is, so assume
                putchar('%');              // it's not a formating string.
                putchar(*p);
                continue;
        }

        if (g->Token == STRING)
            doError( g, E_BADTOK );

        value = Expr( g );                 // Evaluate parameter expression.

        //
        // Now format a number...
        //
        formatNumber(  flags & P_SIGN ? (long) value  :
                                        (unsigned long) value,
                       flags,
                       Prec,
                       Width,
                       Base );
    }


    // Now, if there's more things on statement, handle them like normal basic program...
    if (g->Token != EOL) {

        while (g->Token != EOL) {

            if (g->Token != ',')
                break;

            Lex( g );

            if (g->Token != ID &&
                g->Token != ICON &&
                g->Token != '(' &&
                g->Token != STRING)
                break;

            if (g->Token == STRING) {
                printf("%s", g->tokenText );
                Lex( g );
            } else {
                printf( "%d ", Expr( g ) );
            }
        }
        putchar('\n');
    }


}



#define NDIG  16

//----------------------------------------------------------------------------
// Format and output a number of some base...
//----------------------------------------------------------------------------
int  formatNumber( unsigned long  i,                // Value to print.
                   BYTE           flags,            // Flags.
                   int            Prec,             // Precision.
                   int            Width,            // Width of output.
                   int            Base)             // Number base.
{
    char*          cp;
    char           buf[NDIG+1];


    if( Prec > NDIG)                       // Check for maximum length
        Prec = NDIG;

    if( flags & P_SIGN && (long)i < 0)     // Check sign.
        i = -(long)i;                      // make it positive.
    else
        flags &= ~P_SIGN;

    if(Prec == 0 && i == 0)
        Prec++;

    cp = &buf[NDIG];

    while ( i || Prec > 0) {               // Format each position of the number.
        if (flags & P_UPCASE)
            *--cp = "0123456789ABCDEF"[i%Base];
        else
            *--cp = "0123456789abcdef"[i%Base];
        i /= Base;
        Prec--;
    }

    Prec = (&buf[NDIG] - cp);              // Number of digits we actually formatted.

    //if (flags & P_SIGN)                    // If we're printing sign, add one for it.
    //    Prec++;

    if ( !(flags & P_LEFT))
        while (Width-- > Prec)             // Right justify?
            putchar(' ');
    if (flags & P_SIGN) {                  // Add sign?
        putchar('-');
        Width--; // Prec--;
    }

    cp[Prec] = '\0';                       // Null terminate number string.
    while (*cp != '\0')
        putchar(*cp++);

    if ( (flags & P_LEFT))
        while (Width-- > Prec)
            putchar(' ');

   return 0;
}




//-----------------------------------------------------------------------------
// Done() -- Program has Terminated Normally...
//-----------------------------------------------------------------------------
void Done( global* g )
{
	if (!FlagNoHeading)
		printf( "Done\n" );
    longjmp ( g->error, 1 );
}



//-----------------------------------------------------------------------------
// If() -- Process If .. Then .. Else...
//-----------------------------------------------------------------------------
void If( global* g )
{
    line*   p;
    line*   q;
    I32     val;

    Lex( g );

    val = Expr( g );
    if (g->Token != THEN)
        doError( g, E_NOTHEN );

    Lex( g );

    if (g->Token != ICON || !findLine( g, g->Value, &p, &q ))
        doError( g, E_BADLINE );

    if (val) {
        g->pPC = p;
        g->PC = g->Value;
        g->jump_flag = 1;
        return;
    }

    Lex( g );

    if (g->Token == EOL)
        return;
    if (g->Token != ELSE)
        doError( g, E_NOELSE );
    if (g->Token != ICON || !findLine( g, g->Value, &p, &q ))
        doError( g, E_BADLINE );

    g->pPC = p;
    g->PC = g->Value;
    g->jump_flag = 1;
}



//-----------------------------------------------------------------------------
// Assign() -- Process an assignment statement...
//-----------------------------------------------------------------------------
void Assign( global* g )
{
    char    name[32];

    strcpy( name, g->tokenText );

    Lex ( g );

    if (g->Token != '=')
        doError( g, E_SYNTAX );

    Lex( g );

    storeVar( g, name, Expr( g ) );
}



//-----------------------------------------------------------------------------
//  List() -- List Program...
//-----------------------------------------------------------------------------
void List( global* g )
{
    line    *p, *q;
    int     count;

    Lex( g );

    if (g->Token == ICON) {
        findLine( g, g->Value, &p, &q );
        Lex( g );
        if (g->Token == ICON)
            count = g->Value;
        else
            count = 3;
    } else {
        p = g->head;
        count = -1;
    }
    while (p && count--) {
        printf( "%4d %s\n", p->line_num, p->line_text );
        p = p->next;
    }
}



//-----------------------------------------------------------------------------
//  Save() -- Save Program onto disk...
//-----------------------------------------------------------------------------
void Save( global* g )
{
    line    *p, *q;
    unsigned long  last;
    char*   s;
    char*   t;
    FILE*   fout;

    // Here, we have to extract the file name.  Since we are going to allow
    // a DOS style file name, we need to parse out the name, then reset
    // g->linePtr for Lex()...

    s = g->linePtr;
    while( *s++ != ' ');            // Scoot past whitespace before filename.
    t = g->tokenText;               // We'll just copy filename here.
    while( (*t = *s) != ' ') {      // Copy filename.
        s++; t++;
    }
    *t = '\0';                      // Null terminate filename.
    g->linePtr = s;                 // Restore line pointer past filename.

    if( !isalpha(g->tokenText[0]))
        doError( g, E_BADFILENAME);

    if ((fout = fopen(g->tokenText, "w")) == NULL)
        doError( g, E_CANTOPENFILE);

    Lex( g );

    if (g->Token == ICON) {     // User specified starting line #
        findLine( g, g->Value, &p, &q );
        last = -1;
        Lex( g );
        if (g->Token == ICON)   // User specified ending line #
            last = g->Value;
    } else {                    // Otherwise user wants all lines.
        p = g->head;
        last = -1;
    }

    while (p && (unsigned) p->line_num <= last) {
        fprintf( fout, "%4d %s\n", p->line_num, p->line_text );
        p = p->next;
    }

    fclose( fout );
}



//-----------------------------------------------------------------------------
//  Load() -- Load a program from disk...
//-----------------------------------------------------------------------------
void Load( global* g )
{
    char*   s;
    char*   t;

    // Here, we have to extract the file name.  Since we are going to allow
    // a DOS style file name, we need to parse out the name, then reset
    // g->linePtr for Lex()...

    s = g->linePtr;
    while( *s++ != ' ');            // Scoot past whitespace before filename.
    t = g->tokenText;               // We'll just copy filename here.
    while( (*t = *s) != ' ') {      // Copy filename.
        s++; t++;
    }
    *t = '\0';                      // Null terminate filename.
    g->linePtr = s;                 // Restore line pointer past filename.

    if( !isalpha(g->tokenText[0]))
        doError( g, E_BADFILENAME);

    loadFile(g, g->tokenText);
}



//-----------------------------------------------------------------------------
//  loadFile() -- Do the actuall loading of the program from disk...
//-----------------------------------------------------------------------------
void  loadFile(     global* g, char* filename)
{
    FILE*   fin;
    char*   p;

    if ((fin = fopen(filename, "r")) == NULL)
        doError( g, E_CANTOPENFILE);

    // As we read lines in, we will ignore lines that do not start with a line number
    // and lines that only have a line number in them...
    while ( fgets(g->currentLine, sizeof(g->currentLine), fin) != NULL) {

        g->currentLine[strlen(g->currentLine)-1] = '\0'; // Zero terminate over cr.
        g->linePtr = g->currentLine;

        Lex( g );

        if (g->Token == ICON) {
            p = g->linePtr;
            Lex( g );
            if (g->Token != EOL)
                storeLine(g, g->Value, p);
        }
    }

    fclose(fin);
}




//-----------------------------------------------------------------------------
//  forNext() -- Handle for..next commands...
//-----------------------------------------------------------------------------
void forNext( global* g )
{
    line* saveFORLine;
    line* saveNEXTLine;
    char  controlVar[32];
    BYTE  flag = 0;
#define FLAG_TO  0x01
#define FLAG_BY  0x02
    I32   toVal;
    I32   byVal;

    saveFORLine = g->pPC;
    saveNEXTLine = NULL;

    Lex( g );
    if (g->Token != ID)
        doError(g, E_BADFORVAR);
    strcpy(controlVar, g->tokenText);
    Lex( g );
    if (g->Token == '=') {
        Lex( g );
        storeVar(g, controlVar, Expr( g ));
    }
    // Handle TO and BY expressions...
    while (g->Token != EOL) {
        switch(g->Token) {
            case TO:
                Lex( g );
                flag |= FLAG_TO;
                toVal = Expr( g );
                break;
            case BY:
                Lex( g );
                flag |= FLAG_BY;
                byVal = Expr( g );
                break;
            default:
                doError(g, E_BADFOREXPR);
                break;
        }
    }

    // OK... We should be ready to execute the FOR loop...
    // At the top of the loop we check control var, then
    // execute the commands in the loop. If command is NEXT
    // and if our control var, then do top of loop again...
    while (1) {
        // Check to see of control var has reached TO value...
        if (flag & FLAG_TO) {
            if (byVal < 0) {
                if (readVar(g, controlVar) <= toVal)
                    break;
            } else {
                if (readVar(g, controlVar) >= toVal)
                    break;
            }
        }
        if (gotoNextLine( g ) == NULL)
            doError(g, E_MISSINGNEXT);
        while (1) {
           Lex( g );
           if (g->Token == NEXT) {
               Lex( g );
               if (g->Token != ID)
                   doError(g, E_BADFORVAR);
               if (strcmp(controlVar, g->tokenText) != 0) {  // Not our control var.
                   gotoNextLine( g );                        // Skip NEXT
                   continue;
               }
               // This is our NEXT line. Prepare to go back to top of loop...
               saveNEXTLine = g->pPC;           // Save where NEXT line is.
               g->pPC = saveFORLine;            // GO back to For statement.
               break;                           // Break out to go to top of loop.
           }
           // Not NEXT statement, so execute command.
           if (g->trace_flag)
               printf( "[%d] %s\n", g->PC, g->pPC->line_text);
           g->jump_flag = 0;
           Command( g );
           if (!g->jump_flag)
               gotoNextLine( g );
        }

        // Increment control variable...
        if (flag & FLAG_BY)
            storeVar( g, controlVar, readVar(g, controlVar) + byVal);
        else
            storeVar( g, controlVar, readVar(g, controlVar) + 1);
    }

    // OK, we are breaking out of loop.  If we have noted where the NEXT command
    // is, then we can go right to it.  If not, we have to go find it...
    if (saveNEXTLine) {
        g->pPC = saveNEXTLine;
    } else {
        while (1) {
            if (gotoNextLine( g ) == NULL)
                doError(g, E_MISSINGNEXT);
            Lex( g );
            if (g->Token == NEXT) {
                Lex( g );
                if (g->Token == ID) {
                    if(strcmp(g->tokenText, controlVar) == 0)
                        break;
                } else
                    doError(g, E_BADFORVAR);
            }
        }
    }
}


//-----------------------------------------------------------------------------
// Expr() -- Evaluate Expressions...
//-----------------------------------------------------------------------------
I32 Expr( global* g )
{
    I32     val;

    val = XorExpr( g );
    if (g->Token == '|') {
        Lex( g );
        val |= Expr( g );
    }
    return ( val );
}

I32 XorExpr( global* g )
{
    I32     val;

    val = AndExpr( g );
    if (g->Token == '^') {
        Lex( g );
        val ^= XorExpr( g );
    }
    return ( val );
}

I32 AndExpr( global* g )
{
    I32     val;

    val = NotNegExpr( g );
    if (g->Token == '&') {
        Lex( g );
        val &= AndExpr( g );
    }
    return ( val );
}

I32 NotNegExpr( global* g )
{
    I32     val;

    if (g->Token == '~') {
        Lex( g );
        val = ~ NotNegExpr( g );
    } else if (g->Token == '-') {
        Lex( g );
        val = - NotNegExpr( g );
    } else if (g->Token == '!') {
        Lex( g );
        val = ! NotNegExpr( g );
    } else
        val = CompExpr( g );
    return ( val );
}

I32 CompExpr( global* g )
{
    I32     val;

    val = AddExpr( g );
    switch ( g->Token ) {
        case '=' :       // ==
            Lex( g );
            if (g->Token == '=') {
                Lex( g );
            return ( val == CompExpr( g ) );
            }
            doError(g, E_INVALIDOP);
        case '!' :       // !=
            Lex( g );
            if (g->Token == '=') {
                Lex( g );
                return ( val != CompExpr( g ) );
            }
            doError(g, E_INVALIDOP);
        case '<' :       // < or <=
            Lex( g );
            if (g->Token == '=') {
                Lex( g );
                return ( val <= CompExpr( g ) );
            } else
                return ( val < CompExpr( g ) );
        case '>' :
            if (g->Token == '=') {
                Lex( g );
                return ( val >= CompExpr( g ) );
            } else
            return ( val > CompExpr( g ) );
    }
    return ( val );
}

I32 AddExpr( global* g )
{
    I32     val;
    int     token;

    val = MultExpr( g );

    while (g->Token == '+' ||
           g->Token == '-') {
        token = g->Token;
        Lex( g );
        if (token == '+')
            val += MultExpr( g );
        else if (token == '-')
            val -= MultExpr( g );
    }
    return ( val );
}

I32 MultExpr( global* g )
{
    I32     val, val2;
    int     token;

    val = PrimExpr( g );

    while (g->Token == '*' ||
           g->Token == '/' ||
           g->Token == '%' ) {
        token = g->Token;
        Lex( g );
        if (token == '*')
            val *= PrimExpr( g );
        else if(token == '/') {
            val2 = PrimExpr( g );
            if (! val2)
                doError( g, E_DIV0 );
            val /= val2;
        } else if(token == '%')
            val %= PrimExpr( g );
    }
    return ( val );
}

I32 PrimExpr( global* g )
{
    I32     val;

    switch ( g->Token ) {
        case '(' :
            Lex( g );
            val = Expr( g );
            if (g->Token != ')')
                doError( g, E_NOLPAREN );
            Lex( g );
            return ( val );

        case ICON :
            Lex( g );
            return ( g->Value );

        case ID :
            val = readVar( g, g->tokenText );
            Lex( g );
            return ( val );

        case FUNCTION:
            val = Function( g );
            return ( val );

        default :
            break;
    }

    doError( g, E_SYNTAX );
    return 0;
}



//-----------------------------------------------------------------------------
// storeLine() -- Store Line in Current Program...
//-----------------------------------------------------------------------------
void storeLine (  global* g,
                  int     num,
                  char*   text )
{
    line*    p;
    line*    q;
    line*    s;

    deleteLine( g, num );
    s = (line *) xalloc( g, sizeof(line) );
    s->line_num = num;
    s->line_text = salloc( g, text );
    findLine( g, num, &p, &q );
    if (q)
        q->next = s;
    else
        g->head = s;
    s->next = p;
}



//-----------------------------------------------------------------------------
// deleteLine() -- Delete Selected Line from Current Program...
//-----------------------------------------------------------------------------
void deleteLine ( global* g, int  num)
{
    line*   p;
    line*   q;

    if (findLine( g, num, &p, &q )) {
        if (q)
            q->next = p->next;
        else
            g->head = p->next;
        free( p->line_text );
        free( p );
    }
}



//-----------------------------------------------------------------------------
// findLine() -- Seach for Line given line number...
//-----------------------------------------------------------------------------
int  findLine( global* g,
               int     num,
               line**  hp,
               line**  tp)
{
    line*   p;
    line*   q;

    p = g->head;
    q = NULL;

    while (p) {
        if (p->line_num >= num)
            break;
        q = p;
        p = p->next;
    }

    *hp = p;
    *tp = q;

    if (p)
        return (p->line_num == num);
    else
        return 0;
}



//-----------------------------------------------------------------------------
// findVar() -- Search for variable given name. If Found, return pointer
//              to symbol table entry.  If not Found, create symbol table
//              entry, zero value initially, then return pointer to symbol
//              table entry...
//-----------------------------------------------------------------------------
sym* findVar ( global* g,  char*  name)
{
    sym*    p;
    int     hash;

    p = g->ht[hash = (*name + *(name+1))&(HASH_SIZE-1)];
    while (p && strcmp(p->sym_name,name))
        p = p->next;
    if (p)
        return ( p );
    p = xalloc( g, sizeof(sym) );
    p->sym_name = salloc( g, name );
    p->sym_val  = 0;
    p->next     = g->ht[hash];
    g->ht[hash]    = p;
    return ( p );
}



//-----------------------------------------------------------------------------
// storeVar() -- Store Value in Named Variable...
//-----------------------------------------------------------------------------
void storeVar ( global* g,
                char*    name,
                int      val)
{
    sym*     p;

    p = findVar( g, name );
    p->sym_val = val;
}



//-----------------------------------------------------------------------------
// readVar() -- Returns Value of Named Variable...
//-----------------------------------------------------------------------------
int  readVar ( global* g, char*  name)
{
    sym*    p;

    p = findVar( g, name );
    return ( p->sym_val );
}



//-----------------------------------------------------------------------------
//  Allocate Buffer
//-----------------------------------------------------------------------------
void*  xalloc ( global* g, int  size )
{
    void*    p;

    if ((p = malloc( size )) == NULL)
        doError( g, E_NOMEM );
    return ( p );
}



//-----------------------------------------------------------------------------
//  Copy String to Buffer in Dynamic Memory and Return Pointer              */
//-----------------------------------------------------------------------------
char*  salloc ( global* g, char* s )
{
    char    *p;

    p = (char *) xalloc( g, strlen(s)+1 );
    return ( strcpy( p,s ) );
}




//-----------------------------------------------------------------------------
//  Lex() -- Lexical Analyzer...
//-----------------------------------------------------------------------------

char    *ops = "[])|^&!=#<>+-*/%(,";

#define LEX_CHAR()  ( *g->linePtr )
#define LEX_NEXT()  ( *g->linePtr++)

int  Lex ( global* g )

{   static
    char*       p;
    int         i;

    // Strip leading whitespace...
    while (LEX_CHAR() == ' ' || LEX_CHAR() == '\t')
        LEX_NEXT();

    // Are we at end of line?
    if (LEX_CHAR() == '\r' || !LEX_CHAR())
        return ( g->Token = EOL );

    // Test for operator...
    if (strchr( ops, LEX_CHAR() ))
        return ( g->Token = LEX_NEXT() );

    // Test for identifier...
    if (isalpha( LEX_CHAR() )) {
        p = g->tokenText;

        while (isalnum( LEX_CHAR () ))
            *p++ = LEX_NEXT();

        *p = 0;

        for (i=0; i < sizeof(keyword) / sizeof(keyword[0]); i++)
            if ( !strcmp(keyword[i], g->tokenText) )
                return ( g->Token = i + KEYWORD_START );

        for (i=0; i < sizeof(function) / sizeof(function[0]); i++)
            if ( function[i].func_name != NULL ) {
                if ( !strcmp( function[i].func_name, g->tokenText) ) {
                    g->functionID = &function[i];
                    return ( g->Token = FUNCTION );
                }
            }

        return ( g->Token = ID );
    }

    // test for string...
    if (LEX_CHAR() == '\"') {

        p = g->tokenText;
        g->linePtr++;

        while (LEX_CHAR() && LEX_CHAR() != '\"')
            *p++ = LEX_NEXT();

        if (! LEX_CHAR())
            longjmp ( g->error, E_BADSTRING );

        *p++ = 0;
        g->linePtr++;
        return ( g->Token = STRING );
    }

    // How about a number?
    if ( isdigit(LEX_CHAR()) ) {
        // Hex notation?
        if (LEX_CHAR() == '0') {
            LEX_NEXT();
            if (LEX_CHAR() == 'x' || LEX_CHAR() == 'X') {
                LEX_NEXT();
                g->Value = cnvToHex(g->linePtr, &p);
                g->linePtr = p;
                return ( g->Token = ICON );
            }
        }
        // Otherwise assume decimal...
        g->Value = (I32) strtol ( g->linePtr, &p, 0 );
        g->linePtr = p;
        return ( g->Token = ICON );
    }

    doError( g, E_BADTOK );
    return 0;
}



//-----------------------------------------------------------------------------
// cnvToLong() -- Convert decimal string to integer...
//-----------------------------------------------------------------------------
I32 cnvToLong(char* start, char** end)
{
    I32  i = 0;

    while (1) {
        if (*start >= '0' && *start <= '9') {
            i *= 10;
            i += (*start & 0x0f);
        } else
            break;
        start++;
    }

    *end = start;

    return i;
}



//-----------------------------------------------------------------------------
// cnvToHex() -- Convert hexadecimal string to integer...
//-----------------------------------------------------------------------------
I32 cnvToHex(char* start, char** end)
{
    I32  i = 0;

    while (1) {
        if (*start >= '0' && *start <= '9') {
            i <<= 4;
            i += (*start & 0x0f);
        } else if (*start >= 'a' && *start <= 'f') {
            i <<= 4;
            i += (*start & 0x0f) + 9;
        } else if (*start >= 'A' && *start <= 'F') {
            i <<= 4;
            i += (*start & 0x0f) + 9;
        } else
            break;
        start++;
    }

    *end = start;

    return i;
}


//-----------------------------------------------------------------------------
//  copyInitialProgram() -- Enter the initial program.  Immediately execute
//                          any commands that don't have line nbrs...
//-----------------------------------------------------------------------------
void copyInitialProgram( global* g )
{
    char* s;
    char* p;

    p = initialProgram;

    while (*p != '\0') {
        s = g->currentLine;         // Point to place to copy line.
        while (*p != '\n' && *p != '\0')
            *s++ = *p++;            // Copy a char at a time.
        *s = '\0';                  // Null terminate copied line.
        p++;                        // Step to start of next line.
        g->linePtr = g->currentLine;
        Lex( g );                   // Get first token for Line().
        Line( g );                  // Process this line.
    }
}



//-----------------------------------------------------------------------------
//  peek() -- Look at a 32-bit word of memory at a specific address...
//-----------------------------------------------------------------------------
I32 peek( global* g, I32 i )
{
    I32* p = (I32*) i;
    I32  value;

    value = *p;

    return value;
}

//-----------------------------------------------------------------------------
//  peek2() -- Look at a 16-bit half-word of memory at a specific address...
//-----------------------------------------------------------------------------
I32 peek2( global* g, I32 i )
{
    I16* p = (I16*) i;
    I16  value;

    value = *p;

    return (I32) value;
}

//-----------------------------------------------------------------------------
//  peek1() -- Look at a byte of memory at a specific address...
//-----------------------------------------------------------------------------
I32 peek1( global* g, I32 i )
{
    BYTE* p = (BYTE*) i;
    BYTE  value;

    value = *p;

    return (I32) value;
}

//-----------------------------------------------------------------------------
//  poke() -- Stick a 32-bit word value into a specific memory location...
//-----------------------------------------------------------------------------
I32 poke( global* g, I32 i, I32 x)
{
    I32*  p = (I32*) i;

    *p = x;

    return 0;
}

//-----------------------------------------------------------------------------
//  poke2() -- Stick a 16-bit half-word value into a specific memory location...
//-----------------------------------------------------------------------------
I32 poke2( global* g, I32 i, I32 x)
{
    I16*  p = (I16*) i;

    *p = (U16) x;

    return 0;
}

//-----------------------------------------------------------------------------
//  poke1() -- Stick a byte value into a specific memory location...
//-----------------------------------------------------------------------------
I32 poke1( global* g, I32 i, I32 x)
{
    BYTE*  p = (BYTE*) i;

    *p = (BYTE) x;

    return 0;
}


//-----------------------------------------------------------------------------
//  users() -- Function to list users...
//-----------------------------------------------------------------------------
I32 users( global* g )
{
    listUsers( g );

    return 0;
}



//-----------------------------------------------------------------------------
//  geti() -- Allow user to input an integer...
//-----------------------------------------------------------------------------
I32 geti( global* g )
{
    I32 i;
    char s[32];
    char* p;

    gets(s);
    i = cnvToLong(s, &p);

    return i;
}



//-----------------------------------------------------------------------------
//  login() -- Make user login to system by asking for his name...
//-----------------------------------------------------------------------------
void  login(       global* g )
{
    char   s[256];
    NAME*  Name;
    int    i;

    printf("\n\nPlease enter your name:  ");
    gets(s);
    i = strlen(s);
    s[i] = '\0';  // Kill CR.

    Name = malloc(sizeof(Name));

    Name->Name = malloc(i + 1); // Length of name plus null terminator.
    Name->Next = NULL;
    Name->Prev = NULL;

    strcpy(Name->Name, s);

    if(head == NULL)
        head = Name;
    else {
        Name->Next = head;
        head->Prev = Name;
        head       = Name;
    }

    g->name = Name;

    printf("\nThanks!\n\n");
    printf("Hello %s.\nHere are the other people that are currently logged on:\n", s);
    listUsers( g );

    printf("\n\nNow entering JBASIC. Type \"help\" to get help info about JBASIC\n"
           "Type \"list\" to list default program.\n\n");

}


//-----------------------------------------------------------------------------
//  logout() -- Logoff user...
//-----------------------------------------------------------------------------
void  logout( global* g )
{
    NAME* p;

    // Unchain Name structure...
    p = g->name;

    if( p->Prev) {          // Not at head of list.
        p->Prev->Next = p->Next;
    } else
       head = p->Next;      // No Prev.  We were at head of list.

    if(p->Next)
        p->Next->Prev = p->Prev;

    // Free up Name structure...
    free(p->Name);
    free(p);

}


//-----------------------------------------------------------------------------
//  listUsers() -- List all users currently logged on...
//-----------------------------------------------------------------------------
void  listUsers( global* g )
{
    NAME*  p;

    p = head;
    while(p) {
        printf("%s\n", p->Name);
        p = p->Next;
    }
}
