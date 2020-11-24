/* 
 Fabiano Soares da Silva
 24/11/2020
 Exemplo de tratamento Json retorno de HTTP GET
 */
USING Progress.Json.ObjectModel.ObjectModelParser.
USING Progress.Json.ObjectModel.JsonArray.
USING Progress.Json.ObjectModel.JsonObject.
USING OpenEdge.Net.HTTP.IHttpRequest.
USING OpenEdge.Net.HTTP.IHttpResponse.
USING OpenEdge.Net.HTTP.ClientBuilder.
USING OpenEdge.Net.HTTP.RequestBuilder. 

DEFINE VARIABLE oRequest  AS IHttpRequest NO-UNDO.
DEFINE VARIABLE oResponse AS IHttpResponse NO-UNDO.
DEFINE VARIABLE httpUrl AS CHARACTER   NO-UNDO.
DEFINE VARIABLE responseChar AS LONGCHAR NO-UNDO.
define variable responseJson as JsonObject no-undo.

define temp-table ttWorkOrder
    field actualDropOffTime as character
    field actualPickupTime  as character
    field customerName      as character
    field id                as character
    field mileage           as character
    field organizationId    as character
    field plannedDate       as character
    field product           as character.

FUNCTION LongcharToObject RETURNS JsonObject ( input jsonChar as longchar ) :
    /*  funá∆o que transforma um longchar em objeto json */
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

httpUrl = "http://localhost:59116/api/WorkOrders/fe7c4819-ddee-4e3b-b0f7-e77ff6199c1f".

oRequest = RequestBuilder:Get(httpUrl):Request. 

oResponse = ClientBuilder:Build():Client:Execute(oRequest).

IF oResponse:StatusCode <> 200 THEN DO:
    message
        'ERRO: Falha ao conectar com serviáo'
        skip
        'HTTP Status Code:' oResponse:StatusCode
        skip
        oResponse:StatusReason
        VIEW-AS ALERT-BOX.
    RETURN ERROR.
END.


/* Se o retorno for String, converte para objeto */
if TYPE-OF(oResponse:Entity, OpenEdge.Core.String) then do:
    responseChar = CAST(oResponse:Entity, OpenEdge.Core.String):Value.
    assign responseJson = LongcharToObject(responseChar).
end.
/* Se o retorno j† for objeto */
if TYPE-OF(oResponse:Entity, JsonObject) then do:
    assign responseJson = CAST(oResponse:Entity, JsonObject).
end.

IF responseJson = ? THEN DO:
    message
        'ERRO: Falha ao ler retorno do serviáo'
        skip
        'Sem resposta'
        VIEW-AS ALERT-BOX.
    RETURN ERROR.
END.


EMPTY TEMP-TABLE ttWorkOrder.
CREATE ttWorkOrder.
assign
    ttWorkOrder.actualDropOffTime = responseJson:GetJsonText('actualDropOffTime')
    ttWorkOrder.actualPickupTime  = responseJson:GetJsonText('actualPickupTime')
    ttWorkOrder.customerName      = responseJson:GetJsonText('customerName')
    ttWorkOrder.id                = responseJson:GetJsonText('id')
    ttWorkOrder.mileage           = responseJson:GetJsonText('mileage')
    ttWorkOrder.organizationId    = responseJson:GetJsonText('organizationId')
    ttWorkOrder.plannedDate       = responseJson:GetJsonText('plannedDate')
    ttWorkOrder.product           = responseJson:GetJsonText('product').
                        
find first ttWorkOrder where ttWorkOrder.id <> "" no-error.

if not available ttWorkOrder then do:
    message
        'ERRO: Tabela tempor†ria n∆o populada'
        VIEW-AS ALERT-BOX.
    RETURN ERROR.
end.

MESSAGE
    oResponse:StatusCode 
    SKIP   
    oResponse:StatusReason 
    SKIP
    oResponse:Entity
    skip
    ttWorkOrder.id
    VIEW-AS ALERT-BOX.

