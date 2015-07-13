update
=====

update the rebar3 installation to the newest one published to S3

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your global rebar config:

    {plugins, [
        { update, ".*", {git, "git@host:user/update.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:

    $ rebar3 update
    ===> Fetching update
    ===> Compiling update
    <Plugin Output>