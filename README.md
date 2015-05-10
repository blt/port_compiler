port_compiler
=====

A port compiler for rebar3.

This plugin is intended to replicate the rebar2 support for compiling native
code. It is not a drop-in replacement in terms of command-line interface but the
exact configuration interface in projects' `rebar.config`s have been preserved.

Use In Your Project
---------------------

Add the plugin to your `rebar.config`:

    {plugins, [
        { pc, {git, "git@github.com:blt/port_compiler.git", {branch, "master"}}}
    ]}.
    {provider_hooks,
     [
      {pre,
       [
        {compile, {pc, compile}},
        {clean, {pc, clean}}
       ]
      }
     ]
    }.


From your existing application:


    $ rebar3 pc compile
    ===> Fetching pc
    ===> Compiling pc
    ===> Verifying dependencies...
    Compiling ...

You should now have native code compiled.

Use with Existing Dependency
-----------------------------

If your project depends on a dependency that used the rebar2 port compiler instead of forking and changing the `rebar.config` of that dependency you can use [overrides](http://www.rebar3.org/v3.0/docs/configuration#overrides) to inject the changes from your top level `rebar.config`. Using [jiffy](https://github.com/davisp/jiffy) as an example:


```erlang
{deps, [
  {jiffy, {git, "git@github.com:davisp/jiffy.git", {branch, "master"}}}
]}.

{overrides,
 [{override, jiffy, [
     {plugins, [
         {pc, {git, "git@github.com:blt/port_compiler.git", {branch, "master"}}}
     ]},

     {provider_hooks, [
         {post,
             [
             {compile, {pc, compile}},
             {clean, {pc, clean}}
             ]
          }]
      }
  ]}
]}.
```

Example
---

Looking for an example? See my fork of jiffy here and the changes to its
`rebar.config`: https://github.com/blt/jiffy/commit/d4a0103daec5a646e71045bdf40f12a3eb82ace5

- - -

BELOW HERE BE DRAGONS

```
%% Supported configuration variables:
%%
%% * port_specs - Erlang list of tuples of the forms
%%                {ArchRegex, TargetFile, Sources, Options}
%%                {ArchRegex, TargetFile, Sources}
%%                {TargetFile, Sources}
%%
%% * port_env - Erlang list of key/value pairs which will control
%%              the environment when running the compiler and linker.
%%
%%              By default, the following variables are defined:
%%              CC       - C compiler
%%              CXX      - C++ compiler
%%              CFLAGS   - C compiler
%%              CXXFLAGS - C++ compiler
%%              LDFLAGS  - Link flags
%%              ERL_CFLAGS  - default -I paths for erts and ei
%%              ERL_LDFLAGS - default -L and -lerl_interface -lei
%%              DRV_CFLAGS  - flags that will be used for compiling
%%              DRV_LDFLAGS - flags that will be used for linking
%%              EXE_CFLAGS  - flags that will be used for compiling
%%              EXE_LDFLAGS - flags that will be used for linking
%%              ERL_EI_LIBDIR - ei library directory
%%              DRV_CXX_TEMPLATE  - C++ command template
%%              DRV_CC_TEMPLATE   - C command template
%%              DRV_LINK_TEMPLATE - Linker command template
%%              EXE_CXX_TEMPLATE  - C++ command template
%%              EXE_CC_TEMPLATE   - C command template
%%              EXE_LINK_TEMPLATE - Linker command template
%%              PORT_IN_FILES - contains a space separated list of input
%%                   file(s), (used in command template)
%%              PORT_OUT_FILE - contains the output filename (used in
%%                   command template)
%%
%%              Note that if you wish to extend (vs. replace) these variables,
%%              you MUST include a shell-style reference in your definition.
%%              e.g. to extend CFLAGS, do something like:
%%
%%              {port_env, [{"CFLAGS", "$CFLAGS -MyOtherOptions"}]}
%%
%%              It is also possible to specify platform specific options
%%              by specifying a triplet where the first string is a regex
%%              that is checked against Erlang's system architecture string.
%%              e.g. to specify a CFLAG that only applies to x86_64 on linux
%%              do:
%%
%%              {port_env, [{"x86_64.*-linux", "CFLAGS",
%%                           "$CFLAGS -X86Options"}]}
%%
```
