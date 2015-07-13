-module('up2date_prv').
-include_lib("kernel/include/file.hrl").

-export([init/1, do/1, format_error/1]).
-define(PROVIDER, 'up2date').
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, []},                   % The list of dependencies
            {example, "rebar3 up2date"},   % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "Update rebar3"},
            {desc, "Update rebar3"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Rebar3Path = rebar_state:escript_path(State),
    % Get md5 hex
    Rebar3EscriptMd5 = get_md5(Rebar3Path),
    % Check S3.
    case maybe_fetch_rebar3(Rebar3EscriptMd5) of
	up_to_date ->
	    io:format("Rebar3 is up to date"),
	    {ok, State};
	{saved, TmpRebar3} ->
	    ok = replace_rebar3(rebar_state:escript_path(State), TmpRebar3),
	    io:format("Rebar3 updated"),
	    {ok, State}
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% Internal
get_md5(Rebar3Path) ->
    {ok, Rebar3File} = file:read_file(Rebar3Path),
    Digest = crypto:hash(md5, Rebar3File),
    DigestHex = lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Digest)]),
    string:to_lower(DigestHex).

maybe_fetch_rebar3(Rebar3Md5) ->
    TmpDir = ec_file:insecure_mkdtemp(),
    TmpFile = filename:join(TmpDir, "rebar3"),
    case httpc:request(get, {"https://s3.amazonaws.com/rebar3/rebar3",
			     [{"If-None-Match", Rebar3Md5}]},
		       [], [{stream, TmpFile}, {sync, true}],
		       rebar) of
	{ok, {{_, 304, _}, _, _}} ->
	    up_to_date;
	{ok, saved_to_file} ->
	    {saved, TmpFile}
    end.

replace_rebar3(CurrentRebar3, NewRebar3) ->
    {ok, #file_info{mode = Mode,
		    uid = Uid,
		    gid = Gid}} = file:read_file_info(CurrentRebar3, [mode, uid, gid]),
    ok = file:rename(NewRebar3, CurrentRebar3),
    ok = file:write_file_info(CurrentRebar3, #file_info{mode=Mode,
							uid=Uid,
							gid=Gid}).
