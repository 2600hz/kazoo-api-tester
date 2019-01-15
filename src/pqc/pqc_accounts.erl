%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_accounts).

-export([cleanup_accounts/1, cleanup_accounts/2
        ,create_account/2
        ]).

-include("api_tester.hrl").

-spec create_account(pqc_cb_api:state(), kz_term:ne_binary()) -> binary().
create_account(API, NewAccountName) ->
    Resp = at_accounts:create(API, NewAccountName),
    is_binary(NewAccountId = pqc_cb_response:account_id(Resp))
        andalso allow_number_additions(NewAccountId),
    Resp.

-spec allow_number_additions(kz_term:ne_binary()) -> {'ok', kzd_accounts:doc()}.
allow_number_additions(AccountId) ->
    {'ok', _Account} = kzd_accounts:update(AccountId
                                          ,[{kzd_accounts:path_allow_number_additions(), 'true'}]
                                          ).

-spec cleanup_accounts(kz_term:ne_binaries()) -> 'ok'.
cleanup_accounts(AccountNames) ->
    cleanup_accounts(pqc_cb_api:authenticate(), AccountNames).

-spec cleanup_accounts(pqc_cb_api:state(), kz_term:ne_binaries()) -> 'ok'.
cleanup_accounts(API, AccountNames) ->
    _ = [cleanup_account(API, AccountName) || AccountName <- AccountNames],
    kt_cleanup:cleanup_soft_deletes(?KZ_ACCOUNTS_DB).

-spec cleanup_account(pqc_cb_api:state(), kz_term:ne_binary()) -> 'ok'.
cleanup_account(API, AccountName) ->
    _Attempt = try pqc_cb_search:search_account_by_name(API, AccountName) of
                   ?FAILED_RESPONSE ->
                       lager:error("failed to search for account by name ~s~n", [AccountName]);
                   APIResp ->
                       Data = pqc_cb_response:data(APIResp),
                       case kz_json:get_ne_binary_value([1, <<"id">>], Data) of
                           'undefined' ->
                               check_accounts_db(AccountName);
                           AccountId -> at_accounts:delete(API, AccountId)
                       end
               catch
                   'throw':{'error', 'socket_closed_remotely'} ->
                       lager:error("broke the SUT cleaning up account ~s (~p)~n", [AccountName, API])
               end,
    timer:sleep(1000).% was needed to stop overwhelming the socket, at least locally

check_accounts_db(Name) ->
    AccountName = kzd_accounts:normalize_name(Name),
    ViewOptions = [{'key', AccountName}],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_name">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'error', _E} -> lager:error("failed to list by name: ~p", [_E]);
        {'ok', JObjs} ->
            lager:info("deleting from ~s: ~p~n", [?KZ_ACCOUNTS_DB, JObjs]),
            kz_datamgr:del_docs(?KZ_ACCOUNTS_DB, JObjs)
    end.
