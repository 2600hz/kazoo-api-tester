-module(at_accounts).

-export([create/2, create/3
        ,delete/2
        ]).

-include("api_tester.hrl").

-spec create(kazoo_model:model(), kz_term:ne_binary()) -> binary().
create(Model, NewAccountName) ->
    create(Model, kazoo_model:auth_account_id(Model), NewAccountName).

-spec create(kazoo_model:model(), kz_term:ne_binary(), kz_term:ne_binary()) -> binary().
create(Model, ParentAccountId, NewAccountName) ->
    RequestData = kz_json:from_list([{<<"name">>, NewAccountName}]),
    RequestEnvelope = kz_json:from_list([{<<"data">>, RequestData}]),

    at_util:make_request([201, 500]
                        ,fun kz_http:put/3
                        ,account_url(ParentAccountId)
                        ,at_util:req_headers(Model)
                        ,kz_json:encode(RequestEnvelope)
                        ).

-spec delete(kazoo_model:model(), kz_term:ne_binary()) -> binary().
delete(Model, AccountId) ->
    URL = account_url(AccountId),
    RequestHeaders = at_util:req_headers(Model),

    at_util:make_request([200], fun kz_http:delete/2, URL, RequestHeaders).

-spec account_url(kz_term:ne_binary()) -> string().
account_url(AccountId) ->
    string:join([at_util:base_url(), "accounts", kz_term:to_list(AccountId)], "/").
