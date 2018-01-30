-module(erlivo_call).

-export([new/5, request/1, application/1, node/1, to/1, from/1, gateways/1]).

-record(call, { from :: string()
              , to :: string()
              , gateways :: gateways()
              , application :: application()
              , node :: atom()
              , request :: reference()
              }
       ).
-opaque call() :: #call{}.
-export_type([call/0]).

-type gateways() :: [string()].
-type application() :: atom() | {atom(), atom()}.

-type direction() :: inbound | outbound.

-spec new(atom(), string(), string(), gateways(), application()) -> call().
new(Node, From, To, Gateways, App) ->
	#call{from=From, to=To, gateways=Gateways, application=App
             , node = Node, request=make_ref()}.

-spec request(call()) -> reference().
request(#call{request=Req}) ->
     Req.

-spec application(call()) -> application().
application(#call{application=App}) ->
    App.

-spec node(call()) -> atom().
node(#call{node=Node}) ->
    Node.

-spec from(call()) -> string().
from(#call{from=From}) ->
    From.

-spec to(call()) -> string().
to(#call{to=To}) ->
    To.
                            
-spec gateways(call()) -> [string()].
gateways(#call{gateways=Gs}) ->
    Gs.
   

    
