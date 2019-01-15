-ifndef(API_TESTER_HRL).
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(FAILED_RESPONSE, <<"{}">>).

-define(CONFIG_CAT, <<"api_tester">>).
-define(APP, 'api_tester').

-type api_call() :: {'call', module(), atom(), list()}.
-type api_calls() :: [api_call()].

-type http_response_code() :: pos_integer().
-type http_response_body() :: binary().

-type api_error() :: {'error', http_response_code(), http_response_body()}.
-type api_response() :: {'ok', http_response_body()} | api_error().

-define(API_TESTER_HRL, 'true').
-endif.
