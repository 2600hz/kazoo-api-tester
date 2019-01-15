-module(at_accounts).

-export([create/2, create/3
        ,delete/2
        ]).

-include("api_tester.hrl").

-spec create(pqc_cb_api:state(), kz_term:ne_binary()) -> binary().
create(API, NewAccountName) ->
    create(API, pqc_cb_api:auth_account_id(API), NewAccountName).

-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> binary().
create(API, ParentAccountId, NewAccountName) ->
    RequestData = kz_json:from_list([{<<"name">>, NewAccountName}]),
    RequestEnvelope = pqc_cb_api:create_envelope(RequestData),

    pqc_cb_api:make_request([201, 500]
                           ,fun kz_http:put/3
                           ,account_url(ParentAccountId)
                           ,pqc_cb_api:request_headers(API)
                           ,kz_json:encode(RequestEnvelope)
                           ).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary()) -> binary().
delete(API, AccountId) ->
    URL = account_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API),

    pqc_cb_api:make_request([200], fun kz_http:delete/2, URL, RequestHeaders).

-spec account_url(kz_term:ne_binary()) -> string().
account_url(AccountId) ->
    string:join([pqc_cb_api:v2_base_url(), "accounts", kz_term:to_list(AccountId)], "/").
