/*------------------------------------------------------------------------
  Cidades API
  File: api_cidades.p
  Description: Rotina API de cidades
  Input Parameters: Json
  Output Parameters:Json
  Author: Fabiano Soares
  Created: 27/08/2020
------------------------------------------------------------------------*/
USING Progress.Json.ObjectModel.*.
USING Progress.Json.ObjectModel.ObjectModelParser.
USING Progress.Json.ObjectModel.JsonArray.
USING Progress.Json.ObjectModel.JsonObject.

/* Local Variable Definitions ---                                       */
define variable p_filtro as logical no-undo.
define variable p_cidade as character no-undo.
define variable p_sigla  as character no-undo.
define variable p_estado as character no-undo.
define variable p_pais   as character no-undo.

define temp-table ttCidade
    field cidade          like mgcad.cidade.cidade
    field sigla           like mgcad.cidade.sigla
    field estado          like mgcad.cidade.estado
    field pais            like mgcad.cidade.pais
    field cdn-munpio-ibge like mgcad.cidade.cdn-munpio-ibge serialize-name "cdnMunpioIbge"
    field CallBack        as character
    field mgsRetorno      as character.

/* ************************* Included-Libraries *********************** */
{src/web2/wrap-cgi.i}

/* ************************  Function Implementations ***************** */
FUNCTION LongcharToObject RETURNS JsonObject ( input jsonChar as longchar ) :
    /*  fun‡Æo que transforma um longchar em objeto json */
    define variable jsonInput as JsonObject no-undo.
    define variable objParse as ObjectModelParser no-undo.
    define variable jsonOutput as JsonObject no-undo.  

    assign
        jsonOutput = new JsonObject()
        objParse = new ObjectModelParser()                    
        jsonOutput = cast(objParse:Parse(jsonChar), JsonObject).

    delete object objParse.

    return jsonOutput.
END FUNCTION.

/* ************************  Main Code Block  *********************** */
    /* Carrega os parametros passados pelo JSon */
    run LerParametros.
    
    run process.
    
    /* Process the latest Web event. */
    RUN process-web-request.

/* **********************  Internal Procedures  *********************** */
PROCEDURE LerParametros :
    assign
        p_cidade = get-value(trim("cidade"))
        p_sigla  = get-value(trim("sigla"))
        p_estado = get-value(trim("estado"))
        p_pais   = get-value(trim("pais"))
        p_filtro = logical(get-value(trim("filtro"))).
END PROCEDURE.

PROCEDURE outputHeader :
  output-http-header("Access-Control-Allow-Origin", "*").
  output-content-type ("application/json":U).
END PROCEDURE.

PROCEDURE process :
    create ttCidade.
    assign
        ttCidade.CallBack = "erro"
        ttCidade.mgsRetorno = "Nenhuma cidade localizada!".

    if p_filtro = false then do:
        for each mgcad.cidade
            no-lock:
            create ttCidade.
            buffer-copy mgcad.cidade to ttCidade.
        end.
    end.
    else do:
        /* Buscar cidade pelo nome */
        if p_cidade <> "" then do:
            find first mgcad.cidade no-lock
                where mgcad.cidade.cidade = p_cidade
                no-error.
            if available mgcad.cidade then do:
                create ttCidade.
                buffer-copy mgcad.cidade to ttCidade.
            end.
        end.
        
        /* Buscar cidade pela sigla */
        if p_sigla <> "" then do:
            find first mgcad.cidade no-lock
                where mgcad.cidade.sigla = p_sigla
                no-error.
            if available mgcad.cidade then do:
                create ttCidade.
                buffer-copy mgcad.cidade to ttCidade.
            end.
        end.
        
        /* Buscar cidades de um estado */
        if p_sigla <> "" then do:
            for each mgcad.cidade
                where mgcad.cidade.estado = p_estado
                no-lock:
                create ttCidade.
                buffer-copy mgcad.cidade to ttCidade.
            end.
        end.
        
        /* Buscar cidades de um pais */
        if p_pais <> "" then do:
            for each mgcad.cidade
                where mgcad.cidade.pais = p_pais
                no-lock:
                create ttCidade.
                buffer-copy mgcad.cidade to ttCidade.
            end.
        end.        
    end.
    
    find first ttCidade no-error.
    
    if not available ttCidade then do:
        create ttCidade.
        assign
            ttCidade.CallBack = "erro"
            ttCidade.mgsRetorno = "Nenhuma cidade localizada!".
        next.
    end.
    else
        assign
            ttCidade.CallBack = "ok"
            ttCidade.mgsRetorno = "".
END PROCEDURE.

PROCEDURE process-web-request :
  define variable jsonObjeto as JsonArray no-undo.
  define variable jsonTemp AS JsonObject NO-UNDO.
  DEFINE VARIABLE cJSON       AS LONGCHAR NO-UNDO.
  DEFINE VARIABLE lOK         AS LOGICAL   NO-UNDO.                   
                   
  /* 
   * Output the MIME header and set up the object as state-less or state-aware. 
   * This is required if any HTML is to be returned to the browser.
   */
  RUN outputHeader.
  
  find first ttCidade no-lock no-error.

  ASSIGN lOK = TEMP-TABLE ttCidade:WRITE-JSON("LONGCHAR", cJSON).

  assign jsonObjeto = LongcharToObject(cJSON):GetJsonArray("ttCidade").
  
  jsonObjeto:Write(cJSON).

  jsonTemp = NEW JSONObject().
  jsonTemp:ADD("hasNext", false).
  jsonTemp:ADD("items", jsonObjeto).

  jsonTemp:Write(cJSON).

  /* o prefixo cb significa CallBack */
  {&OUT} STRING(cJSON).
  
END PROCEDURE.

