%% -------------------------------------------------------------------
%%
%% This file contains substantial portions of the original rebar_port_compiler.
%% Special thanks to all the folks that contributed to that effort.
%%
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
%% -------------------------------------------------------------------
-module(pc_compilation).

-export([compile_and_link/2, clean/2]).
-export_type([]).

%%%===================================================================
%%% API
%%%===================================================================

-spec compile_and_link(State :: rebar_state:t(),
                       Specs :: pc_port_specs:spec()) -> ok.
compile_and_link(State, Specs) ->
    %% Compile each of the sources
    NewBins = compile_sources(State, Specs),

    %% Make sure that the target directories exist
    lists:foreach(fun(Spec) ->
                          Target = pc_port_specs:target(Spec),
                          ok = filelib:ensure_dir(filename:join(rebar_state:dir(State), Target))
                  end, Specs),

    %% Only relink if necessary, given the Target
    %% and list of new binaries
    lists:foreach(
      fun(Spec) ->
              Target = pc_port_specs:target(Spec),
              Bins   = pc_port_specs:objects(Spec),
              AllBins = [sets:from_list(Bins),
                         sets:from_list(NewBins)],
              Intersection = sets:intersection(AllBins),
              case needs_link(Target, sets:to_list(Intersection)) of
                  true ->
                      LinkTemplate = select_link_template(Target),
                      Env = pc_port_specs:environment(Spec),
                      Cmd = expand_command(LinkTemplate, Env,
                                           string:join(Bins, " "),
                                           Target),
                      rebar_api:info("Linking ~s", [Target]),
                      rebar_utils:sh(Cmd, [{env, Env}, {cd, rebar_state:dir(State)}]);
                  false ->
                      ok
              end
      end, Specs).

clean(_State, Specs) ->
    lists:foreach(fun(Spec) ->
                          Target = pc_port_specs:target(Spec),
                          Objects = pc_port_specs:objects(Spec),
                          rebar_file_utils:delete_each([Target]),
                          rebar_file_utils:delete_each(Objects),
                          rebar_file_utils:delete_each(port_deps(Objects))
                  end, Specs).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

port_deps(SourceFiles) ->
    [pc_util:replace_extension(O, ".d") || O <- SourceFiles].

%%
%% == compilation ==
%%

compile_sources(Config, Specs) ->
    lists:foldl(
      fun(Spec, NewBins) ->
              Sources = pc_port_specs:sources(Spec),
              Type    = pc_port_specs:type(Spec),
              Env     = pc_port_specs:environment(Spec),
              compile_each(Config, Sources, Type, Env, NewBins)
      end, [], Specs).

compile_each(_Config, [], _Type, _Env, NewBins) ->
    lists:reverse(NewBins);
compile_each(Config, [Source | Rest], Type, Env, NewBins) ->
    Ext = filename:extension(Source),
    Bin = pc_util:replace_extension(Source, Ext, ".o"),
    case needs_compile(Source, Bin) of
        true ->
            Template = select_compile_template(Type, compiler(Ext)),
            Cmd = expand_command(Template, Env, Source, Bin),
            ShOpts = [{env, Env}, return_on_error, {use_stdout, false}, {cd, rebar_state:dir(Config)}],
            exec_compiler(Config, Source, Cmd, ShOpts),
            compile_each(Config, Rest, Type, Env, [Bin | NewBins]);
        false ->
            compile_each(Config, Rest, Type, Env, NewBins)
    end.

%%
%% Choose a compiler variable, based on a provided extension
%%
compiler(".cc")  -> "$CXX";
compiler(".cp")  -> "$CXX";
compiler(".cxx") -> "$CXX";
compiler(".cpp") -> "$CXX";
compiler(".CPP") -> "$CXX";
compiler(".c++") -> "$CXX";
compiler(".C")   -> "$CXX";
compiler(_)      -> "$CC".

expand_command(TmplName, Env, InFiles, OutFile) ->
    Cmd0 = proplists:get_value(TmplName, Env),
    Cmd1 = rebar_api:expand_env_variable(Cmd0, "PORT_IN_FILES", InFiles),
    rebar_api:expand_env_variable(Cmd1, "PORT_OUT_FILE", OutFile).

exec_compiler(_Config, Source, Cmd, ShOpts) ->
    case rebar_utils:sh(Cmd, ShOpts) of
        {error, {_RC, RawError}} ->
            AbsSource = filename:absname(Source),
            rebar_api:info("Compiling ~s", [AbsSource]),
            Error = re:replace(RawError, Source, AbsSource,
                               [{return, list}, global]),
            rebar_api:error("~s", [Error]),
            rebar_api:abort();
        {ok, Output} ->
            rebar_api:info("Compiling ~s", [Source]),
            rebar_api:debug("~s", [Output])
    end.

select_compile_template(drv, Compiler) ->
    select_compile_drv_template(Compiler);
select_compile_template(exe, Compiler) ->
    select_compile_exe_template(Compiler).

select_compile_drv_template("$CC")  -> "DRV_CC_TEMPLATE";
select_compile_drv_template("$CXX") -> "DRV_CXX_TEMPLATE".

select_compile_exe_template("$CC")  -> "EXE_CC_TEMPLATE";
select_compile_exe_template("$CXX") -> "EXE_CXX_TEMPLATE".

needs_compile(Source, Bin) ->
    needs_link(Bin, [Source|bin_deps(Bin)]).

%% NOTE: This relies on -MMD being passed to the compiler and returns an
%% empty list if the .d file is not available.  This means header deps are
%% ignored on win32.
bin_deps(Bin) ->
    [DepFile] = port_deps([Bin]),
    case file:read_file(DepFile) of
        {ok, Deps} ->
            parse_bin_deps(list_to_binary(Bin), Deps);
        {error, _Err} ->
            []
    end.

parse_bin_deps(Bin, Deps) ->
    Ds = re:split(Deps, "\\s*\\\\\\R\\s*|\\s+", [{return, binary}]),
    [D || D <- Ds, D =/= <<>>, D =/= <<Bin/binary,":">>].

%%
%% == linking ==
%%

needs_link(SoName, []) ->
    filelib:last_modified(SoName) == 0;
needs_link(SoName, NewBins) ->
    MaxLastMod = lists:max([filelib:last_modified(B) || B <- NewBins]),
    case filelib:last_modified(SoName) of
        0 ->
            true;
        Other ->
            MaxLastMod >= Other
    end.

select_link_template(Target) ->
    case pc_util:target_type(Target) of
        drv -> "DRV_LINK_TEMPLATE";
        exe -> "EXE_LINK_TEMPLATE"
    end.
