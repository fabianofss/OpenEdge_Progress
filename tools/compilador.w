/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

define temp-table ttProgramas
    field caminho as character format "x(60)"
    field dt-mod  as date
    field tipo    as character
    field nm-comp as character format "x(60)"
    field nome    as character format "x(15)".

define temp-table ttErro
    field linha as character
    field coluna as character
    field erro as character.



/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brw-erro-compilacao

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttErro ttProgramas

/* Definitions for BROWSE brw-erro-compilacao                           */
&Scoped-define FIELDS-IN-QUERY-brw-erro-compilacao ttErro.linha ttErro.coluna ttErro.erro   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw-erro-compilacao   
&Scoped-define SELF-NAME brw-erro-compilacao
&Scoped-define QUERY-STRING-brw-erro-compilacao FOR EACH ttErro
&Scoped-define OPEN-QUERY-brw-erro-compilacao OPEN QUERY {&SELF-NAME} FOR EACH ttErro.
&Scoped-define TABLES-IN-QUERY-brw-erro-compilacao ttErro
&Scoped-define FIRST-TABLE-IN-QUERY-brw-erro-compilacao ttErro


/* Definitions for BROWSE brw-programas                                 */
&Scoped-define FIELDS-IN-QUERY-brw-programas ttProgramas.nome ttProgramas.tipo ttProgramas.caminho   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw-programas   
&Scoped-define SELF-NAME brw-programas
&Scoped-define QUERY-STRING-brw-programas FOR EACH ttProgramas
&Scoped-define OPEN-QUERY-brw-programas OPEN QUERY {&SELF-NAME} FOR EACH ttProgramas.
&Scoped-define TABLES-IN-QUERY-brw-programas ttProgramas
&Scoped-define FIRST-TABLE-IN-QUERY-brw-programas ttProgramas


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brw-erro-compilacao}~
    ~{&OPEN-QUERY-brw-programas}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS edt-contemTexto edt-dir-compilacao ch-tp-r ~
ch-tp-w ch-tp-p ch-tp-i btn-buscar brw-programas btn-compilar ~
btn-remover-todos btn-remover brw-erro-compilacao 
&Scoped-Define DISPLAYED-OBJECTS edt-contemTexto edt-dir-compilacao ch-tp-r ~
ch-tp-w ch-tp-p ch-tp-i 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-buscar 
     LABEL "Buscar arquivos" 
     SIZE 20 BY 1.08.

DEFINE BUTTON btn-compilar 
     LABEL "Compilar programas" 
     SIZE 21 BY 1.

DEFINE BUTTON btn-remover 
     LABEL "Remover selecionado" 
     SIZE 24 BY 1.

DEFINE BUTTON btn-remover-todos 
     LABEL "Remover todos" 
     SIZE 20 BY 1.

DEFINE VARIABLE edt-contemTexto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Buscar programas contando" 
     VIEW-AS FILL-IN 
     SIZE 28.57 BY .88 NO-UNDO.

DEFINE VARIABLE edt-dir-compilacao AS CHARACTER FORMAT "X(256)":U INITIAL "~\~\192.168.0.239~\erp~\DMS3.00~\revendas~\osp~\" 
     LABEL "Diretório Origem" 
     VIEW-AS FILL-IN 
     SIZE 56 BY .88 NO-UNDO.

DEFINE VARIABLE ch-tp-i AS LOGICAL INITIAL no 
     LABEL "*.I" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY .75 NO-UNDO.

DEFINE VARIABLE ch-tp-p AS LOGICAL INITIAL no 
     LABEL "*.P" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY .75 NO-UNDO.

DEFINE VARIABLE ch-tp-r AS LOGICAL INITIAL no 
     LABEL "*.R" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY .75 NO-UNDO.

DEFINE VARIABLE ch-tp-w AS LOGICAL INITIAL yes 
     LABEL "*.W" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY .75 NO-UNDO.

/* Query definitions                                                    */
DEFINE QUERY brw-erro-compilacao FOR 
      ttErro SCROLLING.

DEFINE QUERY brw-programas FOR 
      ttProgramas SCROLLING.

/* Browse definitions                                                   */
DEFINE BROWSE brw-erro-compilacao
  QUERY brw-erro-compilacao DISPLAY
      ttErro.linha format "x(8)"
 ttErro.coluna format "x(8)"
 ttErro.erro
    WITH NO-ROW-MARKERS SEPARATORS SIZE 109 BY 5.67
         TITLE "Erros de compilação" FIT-LAST-COLUMN.

DEFINE BROWSE brw-programas
  QUERY brw-programas DISPLAY
      ttProgramas.nome column-label "Nome"
      ttProgramas.tipo
      ttProgramas.caminho column-label "Local do!Programa"
    WITH NO-ROW-MARKERS SEPARATORS SIZE 109 BY 10.25
         TITLE "Lista de Programas" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     edt-contemTexto AT ROW 1.25 COL 27 COLON-ALIGNED WIDGET-ID 24
     edt-dir-compilacao AT ROW 2.5 COL 27 COLON-ALIGNED WIDGET-ID 20
     ch-tp-r AT ROW 3.75 COL 29 WIDGET-ID 68
     ch-tp-w AT ROW 3.75 COL 35.29 WIDGET-ID 70
     ch-tp-p AT ROW 3.75 COL 42.29 WIDGET-ID 72
     ch-tp-i AT ROW 3.75 COL 48.57 WIDGET-ID 74
     btn-buscar AT ROW 4.67 COL 29 WIDGET-ID 58
     brw-programas AT ROW 6.08 COL 1.86 WIDGET-ID 200
     btn-compilar AT ROW 16.5 COL 2 WIDGET-ID 82
     btn-remover-todos AT ROW 16.5 COL 65.14 WIDGET-ID 80
     btn-remover AT ROW 16.5 COL 86 WIDGET-ID 78
     brw-erro-compilacao AT ROW 17.83 COL 2 WIDGET-ID 300
     "Tipo de arquivo:" VIEW-AS TEXT
          SIZE 15.72 BY .67 AT ROW 3.75 COL 13.29 WIDGET-ID 40
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110.86 BY 22.67 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */

/* *************************  Create Window  ************************** */

IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Compilador programas"
         HEIGHT             = 22.67
         WIDTH              = 110.86
         MAX-HEIGHT         = 34.88
         MAX-WIDTH          = 228.57
         VIRTUAL-HEIGHT     = 34.88
         VIRTUAL-WIDTH      = 228.57
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brw-programas btn-buscar DEFAULT-FRAME */
/* BROWSE-TAB brw-erro-compilacao btn-remover DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.



/* Setting information for Queries and Browse Widgets fields            */

/* Query rebuild information for BROWSE brw-erro-compilacao
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttErro.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brw-erro-compilacao */

/* Query rebuild information for BROWSE brw-programas
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttProgramas.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brw-programas */

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
ON END-ERROR OF C-Win /* Compilador programas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.


ON WINDOW-CLOSE OF C-Win /* Compilador programas */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.


&Scoped-define SELF-NAME btn-buscar
ON CHOOSE OF btn-buscar IN FRAME DEFAULT-FRAME /* Buscar arquivos */
DO:
    DEFINE VARIABLE tipoArquivo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pathBusca AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vAux AS CHARACTER NO-UNDO.
    DEFINE VARIABLE linha AS CHARACTER NO-UNDO.
    DEFINE VARIABLE contemTexto AS LOGICAL NO-UNDO.

    /* Limpa tabela de programas */
    empty temp-table ttProgramas.
    
    /* caminho onde vamos buscar os arquivos */
    assign pathBusca = edt-dir-compilacao:screen-value in frame {&frame-name}.

    if pathBusca = "" then do:
        MESSAGE 
            "Informe um diretório para busca!"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        return.
    end.

    /* Se não foi informado "\" no fim do caminho vamos adicionar */
    if substring(pathBusca,length(pathBusca) - 1, 1) <> "\" then do:
        assign pathBusca = pathBusca + "\".
    end.

    /* Extenções de arquivos que vamos considerar na busca */
    if ch-tp-r:CHECKED in frame {&frame-name} then
        assign tipoArquivo = tipoArquivo + "*.r,".

    if ch-tp-w:CHECKED in frame {&frame-name} then
        assign tipoArquivo = tipoArquivo + "*.w,".

    if ch-tp-p:CHECKED in frame {&frame-name} then
        assign tipoArquivo = tipoArquivo + "*.p,".

    if ch-tp-i:CHECKED in frame {&frame-name} then
        assign tipoArquivo = tipoArquivo + "*.i,".

    /* Vamos usar comando do DOS para listar os arquivos no diretório */
    INPUT  THROUGH  VALUE("dir /b/s " + pathBusca + tipoArquivo).
    REPEAT:
        /* o comando dir vai retornar o caminho completo do arquivo localizado
           ao executar o import o sistema está registrando essa informação na primeira coluna de nossa tabela
           que no caso é ttProgramas.caminho */
        create ttProgramas.
        IMPORT ttProgramas no-error.
        if NOT ERROR-STATUS:ERROR then do:
            /* agora que temos o path completo do arquivo podemos usar o file-info para obter dados deste arquivo */
            ASSIGN FILE-INFO:FILE-NAME = ttProgramas.caminho.

            assign ttProgramas.dt-mod  = DATE(FILE-INFO:FILE-MOD-DATE) no-error. /* Data de modificação */
            /* estraindo o nome entre a ultima barra e o ultimo ponto antes da extenção do arquivo */
            assign vAux = substring(ttProgramas.caminho, r-index(ttProgramas.caminho, '\') + 1).
            assign vAux = substring(vAux, 1, r-index(vAux, '.') - 1).
            assign ttProgramas.nome  = vAux.

            /* Obter a extensão do programa */
            assign vAux = ttProgramas.caminho.
            assign vAux = substring(vAux, r-index(vAux, '.'), (length(vAux) - (r-index(vAux, '.') - 1))).
            assign ttProgramas.tipo  = vAux.
        end.
    END.                                                                                          
    INPUT CLOSE.

    /* Vamos buscar arquivos que contenham um texto em especifico, como o nome de uma tabela por exemplo */
    if edt-contemTexto:screen-value in frame {&frame-name} <> "" then do:

        for each ttProgramas
            where ttProgramas.caminho <> "":
            assign contemTexto = false.
            /* carrega o conteúdo do arquivo */
            input from value(ttProgramas.caminho).
            repeat:
                /* carrega linha a linha do arquivo */
                import UNFORMATTED linha .
                /* se encontrar a chave na linha */
                if index(linha, edt-contemTexto:screen-value in frame {&frame-name}) > 0 then do:
                    assign contemTexto = true.
                    leave.
                end.
            end.
            input close.
            /* Se não encontrar nada então remove o arquivo da lista */
            if contemTexto = false then 
                delete ttProgramas.
        end.
    end.

    for each ttProgramas
        where trim(ttProgramas.caminho) = ""
        :
        delete ttProgramas.
    end.

    /* Atualizar os registros no browse */
    {&OPEN-QUERY-brw-programas}

END.


&Scoped-define SELF-NAME btn-compilar
ON CHOOSE OF btn-compilar IN FRAME DEFAULT-FRAME /* Compilar programas */
DO:
    define variable i        as integer no-undo initial 0.

    for each ttProgramas
        where lookup(ttProgramas.tipo,".w,.p") > 0 /* Vamos compilar apenas arquivos .w e .p */
        :
        /* Executa a compilação do programa */
        compile value(ttProgramas.caminho) save no-error.
        if compiler:error then do:
            do i = 1 to compiler:num-messages:
                create ttErro.
                assign
                    ttErro.linha = string(compiler:get-error-row(i))
                    ttErro.coluna = string(compiler:get-error-column(i))
                    ttErro.erro = compiler:get-message(i).
            end.
        end.
    end.
    
    {&OPEN-QUERY-brw-erro-compilacao}
END.


&Scoped-define SELF-NAME btn-remover
ON CHOOSE OF btn-remover IN FRAME DEFAULT-FRAME /* Remover selecionado */
DO:
    if available ttProgramas then do:
        delete ttProgramas.
        {&OPEN-QUERY-brw-programas}
    end.
END.


&Scoped-define SELF-NAME btn-remover-todos
ON CHOOSE OF btn-remover-todos IN FRAME DEFAULT-FRAME /* Remover todos */
DO:
    for each ttProgramas:
        delete ttProgramas.
    end.
    {&OPEN-QUERY-brw-programas}
END.


&Scoped-define BROWSE-NAME brw-erro-compilacao
&UNDEFINE SELF-NAME



/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.


/* **********************  Internal Procedures  *********************** */

PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY edt-contemTexto edt-dir-compilacao ch-tp-r ch-tp-w ch-tp-p ch-tp-i 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE edt-contemTexto edt-dir-compilacao ch-tp-r ch-tp-w ch-tp-p ch-tp-i 
         btn-buscar brw-programas btn-compilar btn-remover-todos btn-remover 
         brw-erro-compilacao 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.
