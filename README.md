port_compiler
=====

a rebar3 port compiler for native code

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { port_compiler, ".*", {git, "git@host:user/port_compiler.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 port_compiler
    ===> Fetching port_compiler
    ===> Compiling port_compiler
    <Plugin Output>
