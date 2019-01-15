-module(at_util).

-export([base_url/0
        ,req_headers/1
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

-spec req_headers(map()) -> kz_http:headers().
req_headers(Options) ->
    [{"content-type", "application/x-www-form-urlencoded"}
    ,{"accept", "application/json"}
     | request_specific(Options)
    ].

request_specific(#{'request_id' := RequestId}) ->
    [{"x-request-id", kz_term:to_list(RequestId)}];
request_specific(_) -> [].
