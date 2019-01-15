%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_accounts).

-export([create_account/2]).

-include("api_tester.hrl").

-spec create_account(pqc_cb_api:state(), kz_term:ne_binary()) -> api_response().
create_account(API, NewAccountName) ->
    case at_accounts:create(API, NewAccountName) of
        {'error', _, _}=Error -> Error;
        {'ok', Resp}=OK ->
            NewAccountId = kz_json:get_ne_binary_value([<<"data">>, <<"id">>], kz_json:decode(Resp)),
            allow_number_additions(NewAccountId),
            OK
    end.

-spec allow_number_additions(kz_term:ne_binary()) -> {'ok', kzd_accounts:doc()}.
allow_number_additions(AccountId) ->
    {'ok', _Account} = kzd_accounts:update(AccountId
                                          ,[{kzd_accounts:path_allow_number_additions(), 'true'}]
                                          ).
