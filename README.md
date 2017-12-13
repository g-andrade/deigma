

# bottlenape #

[![Build Status](https://travis-ci.org/g-andrade/bottlenape.png?branch=master)](https://travis-ci.org/g-andrade/bottlenape)
[![Hex pm](http://img.shields.io/hexpm/v/bottlenape.svg?style=flat)](https://hex.pm/packages/bottlenape)


### <a name="bottlenape_-_Rate_limited_event_sampler">bottlenape - Rate limited event sampler</a> ###


#### <a name="Description">Description</a> ####

`bottlenape` determines the instantaneous[1] sampling rate of generic events, given particular constraints on rate limiting.

It's destined to situations where it's desirable to sample events but only after they become frequent enough
(e.g. reporting metrics.)

[1]: The system takes a few hundred milliseconds to react to sudden changes in the rate of an event, e.g. during bursts.


#### <a name="Example_Flow">Example Flow</a> ####

```erlang

{ok, _Pid} = bottlenape:start_link(metrics_reporting),
{ok, 1.0} = bottlenape:hit(metrics_reporting, http_request),
{ok, 1.0} = bottlenape:hit(metrics_reporting, http_request),
% ... rate of HTTP requests keeps increasing ...
{ok, 0.7} = bottlenape:hit(metrics_reporting, http_request),
{ok, 0.7} = bottlenape:hit(metrics_reporting, http_request),
drop = bottlenape:hit(metrics_reporting, http_request),
{ok, 0.55} = bottlenape:hit(metrics_reporting, http_request),
drop = bottlenape:hit(metrics_reporting, http_request),
drop = bottlenape:hit(metrics_reporting, http_request),
% ... rate of HTTP requests decreases ...
{ok, 0.8} = bottlenape:hit(metrics_reporting, http_request),
{ok, 0.8} = bottlenape:hit(metrics_reporting, http_request),
drop = bottlenape:hit(metrics_reporting, http_request),
{ok, 0.8} = bottlenape:hit(metrics_reporting, http_request),
{ok, 1.0} = bottlenape:hit(metrics_reporting, http_request).

```


#### <a name="Installation_(Erlang)">Installation (Erlang)</a> ####

Add `bottlenape` to your list of dependencies in `rebar.config`:

```erlang

{deps,
 [{bottlenape, "1.0.0"}
 ]}.

```

And then run `rebar3 compile`


#### <a name="Installation_(Elixir)">Installation (Elixir)</a> ####

Add `bottlenape` to your list of dependencies in `mix.exs`:

```elixir

def deps do
[
  {:bottlenape, "1.0.0"}
]
end

```

And then run `mix deps.get`


#### <a name="Requirements">Requirements</a> ####

The library has been tested on Erlang/OTP versions 19.{0..3}, and 20.{0..1}. The supported build tool is `rebar3`.


#### <a name="More_info">More info</a> ####
See API reference indexed below.


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/g-andrade/bottlenape/blob/master/doc/bottlenape.md" class="module">bottlenape</a></td></tr></table>

