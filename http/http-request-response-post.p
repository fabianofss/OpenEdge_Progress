
BLOCK-LEVEL ON ERROR UNDO, THROW.

USING OpenEdge.Core.String.
USING OpenEdge.Net.HTTP.ClientBuilder.
USING OpenEdge.Net.HTTP.IHttpClient.
USING OpenEdge.Net.HTTP.IHttpRequest.
USING OpenEdge.Net.HTTP.RequestBuilder.
USING OpenEdge.Net.HTTP.IHttpResponse.
USING Progress.Json.ObjectModel.JsonObject.

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE oClient   AS IHttpClient   NO-UNDO.
DEFINE VARIABLE oRequest  AS IHttpRequest  NO-UNDO.
DEFINE VARIABLE oResponse AS IHttpResponse NO-UNDO.

DEFINE VARIABLE oWorkOrder  AS JsonObject    NO-UNDO.
DEFINE VARIABLE oVinProduct  AS JsonObject    NO-UNDO.
DEFINE VARIABLE oSerialNumberProduct  AS JsonObject    NO-UNDO.

DEFINE VARIABLE lJson as LONGCHAR NO-UNDO.

/* ***************************  Main Block  *************************** */
oClient = ClientBuilder:Build():Client.

oVinProduct = NEW JsonObject().
oVinProduct:add('vin','9BSR6X400H3905015').

oSerialNumberProduct = NEW JsonObject().
oSerialNumberProduct:add('scania',true).
oSerialNumberProduct:add('serialNumber','3905015').

oWorkOrder = NEW JsonObject().
oWorkOrder:add('organizationId','25afe3f0-d248-41b8-8716-dcc64c79e2e5').
oWorkOrder:add('plannedDate','2020-12-09').
oWorkOrder:add('vinProduct',oVinProduct).
oWorkOrder:add('serialNumberProduct',oSerialNumberProduct).
/*
/* Visualizar Json formatado */
oWorkOrder:Write(lJson, TRUE).

MESSAGE lJson
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
*/
oRequest = RequestBuilder:Post('http://localhost:59116/api/WorkOrders', oWorkOrder):Request.
oResponse = oClient:Execute(oRequest).

MESSAGE
    oResponse:StatusCode
    skip
    oResponse:StatusReason
    VIEW-AS ALERT-BOX.

CATCH oError AS Progress.Lang.Error :    
    MESSAGE
        oError:GetMessage(1)
        skip
        oError:CallStack
        VIEW-AS ALERT-BOX.
END CATCH.
