-module(kazoo_model).

%% Create
-export([new/1]).

%% Accessors
-export([auth_token/1
        ,auth_account_id/1
        ]).

%% Manipulate the model
-export([add_account_to_model/2]).

-type account_name() :: kz_term:ne_binary().
-type accounts() :: #{account_name() => kzd_accounts:doc()}.

-type model() :: #{'auth_account_id' := kz_term:ne_binary()
                  ,'accounts' := accounts()
                  ,'auth_token' := kz_term:ne_binary()
                  ,'test_id' := kz_term:ne_bniary()
                  }.

-include("api_tester.hrl").

-spec new(kz_term:ne_binary()) -> model().
new(AuthAccountId) ->
    {'ok', AuthAccountJObj} = kzd_accounts:fetch(AuthAccountId),

    Model = new(),
    APIKey = kzd_accounts:api_key(AuthAccountJObj),

    AuthToken = at_api_auth:get_auth_token(Model, APIKey),

    lager:info("STATE INITIALIZED for ~s", [AuthAccountId]),
    Model#{'auth_account_id' => AuthAccountId
          ,'accounts' => add_account_to_accounts(AuthAccountJObj, #{})
          ,'auth_token' => AuthToken
          }.

new() ->
    #{'test_id' => kz_util:get_callid()}.

-spec auth_token(model()) -> kz_term:ne_binary().
auth_token(#{'auth_token' := AuthToken}) -> AuthToken.

-spec auth_account_id(model()) -> kz_term:ne_binary().
auth_account_id(#{'auth_account_id' := AuthAccountId}) -> AuthAccountId.

-spec add_account_to_model(model(), kzd_accounts:doc()) -> model().
add_account_to_model(#{'accounts' := Accounts}=Model, AccountJObj) ->
    Model#{'accounts' => add_account_to_accounts(AccountJObj, Accounts)}.

-spec add_account_to_accounts(kzd_accounts:doc(), accounts()) -> accounts().
add_account_to_accounts(AccountJObj, Accounts) ->
    Name = kzd_accounts:name(AccountJObj),

    Accounts#{Name => AccountJObj}.
