-module(at_api_auth).

-export([get_auth_token/2]).

-include("api_tester.hrl").

-spec get_auth_token(kazoo_model:model(), kz_term:ne_binary()) -> api_response().
get_auth_token(Model, APIKey) ->
    RequestData = kz_json:from_list([{<<"api_key">>, APIKey}]),
    RequestEnvelope = kz_json:from_list([{<<"data">>, RequestData}]),

    at_util:make_request([201]
                        ,fun kz_http:put/3
                        ,api_auth_url()
                        ,at_util:req_headers(Model)
                        ,kz_json:encode(RequestEnvelope)
                        ).

-spec api_auth_url() -> string().
api_auth_url() ->
    BaseURL = at_util:base_url(),
    string:join([BaseURL, "api_auth"], "/").
