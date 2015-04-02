port_compiler
=====

A port compiler for rebar3.

This plugin is intended to replicate the rebar2 support for compiling native
code. It is not a drop-in replacement in terms of command-line interface but the
exact configuration interface in projects' `rebar.config`s have been preserved.

Use
---

Add the plugin to your `rebar.config`:

    {plugins, [
        { pc, {git, "git@github.com:blt/port_compiler.git", {tag, "0.1.0"}}}
    ]}.

From your existing application:


    $ rebar3 pc compile
    ===> Fetching pc
    ===> Compiling pc
    ===> Verifying dependencies...
    Compiling ...

You should now have native code compiled.
