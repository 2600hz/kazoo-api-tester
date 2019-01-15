-module(at_util).

-export([base_url/0
        ,req_headers/1

        ,make_request/4, make_request/5
        ]).

-include("api_tester.hrl").

-spec base_url() -> string().
base_url() ->
    base_url(kapps_config:get_string(?CONFIG_CAT, <<"base_url">>)).

base_url('undefined') ->
    DefaultIP = kz_network_utils:default_binding_ip(),
    IP = kapps_config:get_string(<<"crossbar">>, <<"ip">>, DefaultIP),
    IPAddress = kz_network_utils:get_supported_binding_ip(IP, DefaultIP),

    base_url(IPAddress, kapps_config:get_is_true(<<"crossbar">>, <<"use_ssl">>, 'false'));
base_url(BaseURL) -> BaseURL.

base_url(IPAddress, 'false') ->
    Port = kapps_config:get_integer(<<"crossbar">>, <<"port">>, 8000),
    build_base_url(IPAddress, "http://", Port);
base_url(IPAddress, 'true') ->
    Port = kapps_config:get_integer(<<"crossbar">>, <<"ssl_port">>, 8443),
    build_base_url(IPAddress, "https://", Port).

build_base_url(IPAddress, Scheme, Port) ->
    URL = iolist_to_binary([Scheme, IPAddress, ":", integer_to_list(Port), "/v2"]),
    kapps_config:set_default(?CONFIG_CAT, <<"base_url">>, URL),
    binary_to_list(URL).

-spec req_headers(kazoo_model:model()) -> kz_http:headers().
req_headers(Options) ->
    [{"content-type", "application/x-www-form-urlencoded"}
    ,{"accept", "application/json"}
     | request_specific(Options)
    ].

request_specific(#{'request_id' := RequestId}) ->
    [{"x-request-id", kz_term:to_list(RequestId)}];
request_specific(#{'test_id' := RequestId}) ->
    [{"x-request-id", kz_term:to_list(RequestId)}];
request_specific(_) -> [].


-type expected_code() :: 200..600.
-type expected_codes() :: [expected_code()].
-type expected_headers() :: [{string(), string()}].
-type expectations() :: #{'response_codes' => expected_codes()
                         ,'response_headers' => expected_headers()
                         }.

-type response() :: binary() |
                    kz_http:ret() |
                    {'error', binary()}.

-type fun_2() :: fun((string(), kz_term:proplist()) -> kz_http:ret()).
-type fun_3() :: fun((string(), kz_term:proplist(), iodata()) -> kz_http:ret()).

-spec make_request(expectations() | expected_code() | expected_codes(), fun_2(), string(), kz_term:proplist()) ->
                          response().
make_request(Code, HTTP, URL, RequestHeaders) when is_integer(Code) ->
    make_request(#{'response_codes' => [Code]}, HTTP, URL, RequestHeaders);
make_request([Code|_]=Codes, HTTP, URL, RequestHeaders) when is_integer(Code) ->
    make_request(#{'response_codes' => Codes}, HTTP, URL, RequestHeaders);
make_request(Expectations, HTTP, URL, RequestHeaders) ->
    lager:info("~p: ~s", [HTTP, URL]),
    lager:debug("headers: ~p", [RequestHeaders]),
    handle_response(Expectations, HTTP(URL, RequestHeaders)).

-spec make_request(expectations() | expected_code() | expected_codes(), fun_3(), string(), kz_term:proplist(), iodata()) ->
                          response().
make_request(Code, HTTP, URL, RequestHeaders, RequestBody) when is_integer(Code) ->
    make_request(#{'response_codes' => [Code]}, HTTP, URL, RequestHeaders, RequestBody);
make_request([Code|_]=Codes, HTTP, URL, RequestHeaders, RequestBody) when is_integer(Code) ->
    make_request(#{'response_codes' => Codes}, HTTP, URL, RequestHeaders, RequestBody);
make_request(Expectations, HTTP, URL, RequestHeaders, RequestBody) ->
    lager:info("~p: ~s", [HTTP, URL]),
    lager:debug("headers: ~p", [RequestHeaders]),
    lager:debug("body: ~s", [RequestBody]),
    handle_response(Expectations, HTTP(URL, RequestHeaders, iolist_to_binary(RequestBody))).

-spec handle_response(expectations(), kz_http:ret()) -> response().
handle_response(Expectations, {'ok', ActualCode, RespHeaders, RespBody}) ->
    case expectations_met(Expectations, ActualCode, RespHeaders) of
        'true' ->
            lager:debug("resp headers: ~p", [RespHeaders]),
            RespBody;
        'false' ->
            {'error', RespBody}
    end;
handle_response(_Expectations, {'error','socket_closed_remotely'}=E) ->
    lager:error("~nwe broke crossbar!"),
    throw(E);
handle_response(_ExpectedCode, {'error', _}=E) ->
    lager:error("broken req: ~p", [E]),
    E.

expectations_met(Expectations, RespCode, RespHeaders) ->
    response_code_matches(Expectations, RespCode)
        andalso response_headers_match(Expectations, RespHeaders).

response_code_matches(#{'response_codes' := ResponseCodes}, ResponseCode) ->
    case lists:member(ResponseCode, ResponseCodes) of
        'true' -> 'true';
        'false' ->
            lager:error("failed expectation: code ~w but expected ~w"
                       ,[ResponseCode, ResponseCodes]
                       ),
            'false'
    end;
response_code_matches(_Expectations, _Code) -> 'true'.

response_headers_match(#{'response_headers' := ExpectedHeaders}, RespHeaders) ->
    lists:all(fun(ExpectedHeader) -> response_header_matches(ExpectedHeader, RespHeaders) end
             ,ExpectedHeaders
             );
response_headers_match(_Expectations, _RespHeaders) -> 'true'.

response_header_matches({ExpectedHeader, ExpectedValue}, RespHeaders) ->
    case props:get_value(ExpectedHeader, RespHeaders) of
        ExpectedValue -> 'true';
        'undefined' ->
            lager:error("failed expectation: header ~s missing from response", [ExpectedHeader]),
            'false';
        _ActualValue ->
            lager:error("failed expectation: header ~s is not ~p but ~p"
                       ,[ExpectedHeader, ExpectedValue, _ActualValue]
                       ),
            'false'
    end.
