%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Runs tests against the Crossbar API using all available PQC modules
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(run_api).
-behaviour(proper_statem).

%% PropEr Statem callbacks
-export([command/1
        ,initial_state/0
        ,next_state/3
        ,postcondition/3
        ,precondition/2

        ,correct/0
        ,correct_parallel/0
        ]).

%% manual run
-export([run_counterexample/0, run_counterexample/1
        ,cleanup/0
        ,pqc_modules/0
        ]).

%% PQC Commands
-export([have_connectivity/1]).

-include_lib("proper/include/proper.hrl").
-include("api_tester.hrl").

-spec correct() -> any().
correct() ->
    _ = setup(),
    ?FORALL(Cmds
           ,commands(?MODULE)
           ,?TRAPEXIT(
               begin
                   try run_commands(?MODULE, Cmds) of
                       {History, Model, Result} ->
                           cleanup(),
                           ?WHENFAIL(output_failure(Model, Cmds, History, Result)
                                    ,aggregate(command_names(Cmds), Result =:= 'ok')
                                    )
                   catch
                       E:R ->
                           ST = erlang:get_stacktrace(),
                           io:format('user', "~s: ~p~n~p~n~p~n", [E, R, ST, Cmds])
                   end
               end
              )
           ).

output_failure(Model, Cmds, [], Result) ->
    io:format('user'
             ,"Final Model:~n~p~n~nFailing Cmds:~n~p~nResult: ~p~n"
             ,[Model, Cmds, Result]
             );
output_failure(Model, Cmds, History, Result) ->
    io:format('user'
             ,"Final Model:~n~p~n~nFailing Cmds:~n~p~nResult: ~p~nFailing command: ~p~n"
             ,[Model, zip(Cmds, History), Result
              ,lists:nth(length(History), Cmds)
              ]
             ).

setup() ->
    TestId = kz_binary:rand_hex(4),
    kz_util:put_callid(TestId),

    lager:debug("cleaning up before setup"),
    catch cleanup(),

    lager:info("SETUP COMPLETE").

-spec correct_parallel() -> any().
correct_parallel() ->
    _ = setup(),
    ?FORALL(Cmds
           ,parallel_commands(?MODULE)
           ,?TRAPEXIT(
               begin
                   {Seq, Par, Result} = run_parallel_commands(?MODULE, Cmds),

                   ?WHENFAIL(io:format('user', "R: ~p~nS: ~p~nP: ~p~n", [Result, Seq, Par])
                            ,aggregate(command_names(Cmds), Result =:= 'ok')
                            )
               end
              )
           ).

-type model() :: #{}.

-spec initial_state() -> model().
initial_state() ->
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),

    kazoo_model:new(MasterAccountId).

-spec command(model()) -> proper_types:type().
command(Model) ->
    APICalls = lists:flatten([Module:api_calls(Model) || Module <- pqc_modules()]),
    frequency([{1, {'call', ?MODULE, 'have_connectivity', [Model]}}
               | [{10, Call} || Call <- APICalls]
              ]).

-spec pqc_modules() -> kz_term:atoms().
pqc_modules() ->
    {'ok', Modules} = application:get_key(?APP, 'modules'),
    [Module || Module <- Modules,
               is_pqc_module(kz_term:to_binary(Module))
                   andalso kz_module:is_exported(Module, 'api_calls', 1)
    ].

-spec is_pqc_module(kz_term:ne_binary()) -> boolean().
is_pqc_module(<<"pqc_", _/binary>>) -> 'true';
is_pqc_module(_) -> 'false'.

-spec precondition(model(), api_call()) -> 'true'.
precondition(_Model, {'call', ?MODULE, _F, _A}) -> 'true';
precondition(Model, {'call', Module, _F, _A}=Call) ->
    Module:precondition(Model, Call).

-spec postcondition(model(), api_call(), api_response()) ->
                           boolean().
postcondition(_Model, {'call', ?MODULE, _F, _As}, Result) ->
    Result;
postcondition(Model, {'call', Module, _F, _As}=Call, Result) ->
    Module:check_response(Model, Call, Result).

-spec next_state(model(), api_response(), api_call()) ->
                        model().
next_state(Model, _Result, {'call', ?MODULE, _F, _As}) -> Model;
next_state(Model, Result, {'call', Module, _F, _As}=Call) ->
    Module:update_model(Model, Result, Call).

-spec cleanup() -> 'ok'.
cleanup() ->
    lists:foreach(fun cleanup_pqc/1, pqc_modules()),
    kt_cleanup:cleanup_soft_deletes(?KZ_ACCOUNTS_DB),
    lager:info("CLEANUP DONE").

cleanup_pqc(Module) ->
    Module:cleanup(),
    lager:debug("~s cleaned up", [Module]).

-spec run_counterexample() -> 'ok'.
run_counterexample() ->
    run_counterexample(proper:counterexample()).

-spec run_counterexample('undefined' | list()) -> 'ok'.
run_counterexample('undefined') ->
    io:format("no counterexample, run proper qc~n");
run_counterexample(Commands) when is_list(Commands) ->
    _ = setup(),
    Model = initial_state(),
    io:format('user', "initial state: ~p~n", [Model]),
    case run_counterexample(Model, Commands) of
        'ok' -> io:format("FAILURE~n");
        {_Step, _FinalModel, _QCVars} ->
            io:format("final state: ~p~nSUCCESS~n", [_FinalModel])
    end,
    cleanup().

run_counterexample(Model, [{Seq, Threads}]) ->
    Steps = lists:usort(fun sort_steps/2, Seq ++ lists:flatten(Threads)),
    lists:foldl(fun run_step/2, {0, Model, #{}}, Steps);
run_counterexample(Model, [Steps]) ->
    lists:foldl(fun run_step/2, {0, Model, #{}}, Steps).

sort_steps({'set', Var1, _Call1}, {'set', Var2, _Call2}) ->
    Var1 < Var2.

-type run_acc() :: {non_neg_integer(), model(), #{integer() => any()}}.
-spec run_step(tuple(), run_acc() | 'ok') -> run_acc() | 'ok'.
run_step(_, 'ok') -> 'ok';
run_step({'set', Var, {'call', M, F, [_OldModel | As]}}, {Step, Model, QCVars}=Acc) ->
    Args = proper_symb:eval(QCVars, [Model | As]),

    io:format('user', "~p: ~p:~p(~s): ", [Step, M, F, printable_args(Args)]),
    try apply(M, F, Args) of
        SUTResponse ->
            eval_step(Var, M, F, Args, Acc, SUTResponse)
    catch
        _E:_R ->
            ST = erlang:get_stacktrace(),
            io:format('user', "~n~s: ~p~n~p~n~p~n"
                     ,[_E, _R, ST, Model]
                     )
    end.

eval_step(Var, M, F, Args, {Step, Model, QCVars}, SUTResponse) ->
    io:format('user', "~s~n", [printable_sut_response(SUTResponse)]),

    try postcondition(Model, {'call', M, F, Args}, SUTResponse) of
        'true' ->
            {Step+1
            ,next_state(Model, SUTResponse, {'call', M, F, Args})
            ,QCVars#{Var => SUTResponse}
            };
        'false' ->
            io:format('user', "postcondition failed~n", [])
    catch
        'throw':Error ->
            io:format('user', "postcondition crashed: ~p~n", [Error])
    end.

printable_args([Model | Args]) ->
    kz_binary:join([printable_model(Model) | Args], <<", ">>).

printable_model(#{'accounts' := Accounts
                 ,'test_id' := TestId
                 }
               ) ->
    io_lib:format("{test_id: ~s accounts: ~s}"
                 ,[TestId, kz_binary:join(Accounts, <<", ">>)]
                 ).

printable_sut_response({'ok', _}=Resp) ->
    io_lib:format("~p", [Resp]);
printable_sut_response({'error', Code, Body}) ->
    io_lib:format("error: ~p: ~s", [Code, Body]);
printable_sut_response(Resp) ->
    kz_json:encode(Resp).

-spec have_connectivity(kazoo_model:model()) -> boolean().
have_connectivity(Model) ->
    URL = at_util:base_url(),
    lager:debug("GET ~s", [URL]),
    case kz_http:get(URL, at_util:req_headers(Model)) of
        {'ok', 200, _Headers, _Body} ->
            lager:debug("200: ~p, ~p", [_Headers, _Body]),
            'true';
        {'ok', _Code, _Headers, _Body} ->
            lager:error("~p: ~p~s", [_Code, _Headers, _Body]),
            'false'
    end.
