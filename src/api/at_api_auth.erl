-module(at_api_auth).

-export([get_auth_token/1]).

-include("api_tester.hrl").

-spec get_auth_token(kz_term:ne_binary()) -> binary().
get_auth_token(APIKey) ->
    RequestData = kz_json:from_list([{<<"api_key">>, APIKey}]),
    RequestEnvelope = kz_json:from_list([{<<"data">>, RequestData}]),

    pqc_cb_api:make_request([201]
                           ,fun kz_http:put/3
                           ,api_auth_url()
                           ,pqc_cb_api:request_headers()
                           ,kz_json:encode(RequestEnvelope)
                           ).

-spec api_auth_url() -> string().
api_auth_url() ->
    BaseURL = at_util:base_url(),
    string:join([BaseURL, "api_auth"]).
