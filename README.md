# deigma

[![](https://img.shields.io/hexpm/v/deigma.svg?style=flat)](https://hex.pm/packages/deigma)
[![](https://github.com/g-andrade/deigma/workflows/build/badge.svg)](https://github.com/g-andrade/deigma/actions?query=workflow%3Abuild)

`deigma` is an event sampler for Erlang/OTP and Elixir.

It performs sampling of reported events within continuous one second
windows\[\*\].

The sampling percentage is steadily adjusted so that the events that
seep through are representative of what's happening in the system while
not exceeding specified rate limits.

The sampling percentage is also exposed in the context of each reported
event, so that whichever other component that later receives the samples
can perform reasonable guesses of the original population properties
with limited information.

\[\*\] As far as the [monotonic
clock](http://erlang.org/doc/apps/erts/time_correction.html#Erlang_Monotonic_Time)
resolution goes.

#### Example (Erlang)

There's a heavy duty web service; we want to report metrics on inbound
HTTP requests to a [StatsD](https://github.com/etsy/statsd) service over
UDP while minimising the risk of dropped datagrams due to an excessive
amount of them.

For this, we can downsample the reported metrics while determining the
real sampling percentage using `deigma`.

##### 1\. Start a deigma instance

``` erlang
Category = metrics,
{ok, _Pid} = deigma:start(Category).
```

##### 2\. Sample events

``` erlang
Category = metrics,
EventType = http_request,

case deigma:ask(Category, EventType) of
    {sample, SamplingPercentage} ->
        your_metrics:report(counter, EventType, +1, SamplingPercentage);
    {drop, _SamplingPercentage} ->
        ok
end.
```

  - [`Category`](#categories) must be an atom
  - [`EventType`](#event-windows) can be any term
  - `SamplingPercentage` is a floating point number between 0.0 and 1.0
    representing the percentage of events that were sampled during the
    last 1000 milliseconds, **including** the event reported just now.
  - The rate limit defaults to 100 `EventType` occurences per second
    within a `Category`; it can be [overridden](#rate-limiting).
  - The function invoked each time an event gets registered can also be
    [customized](#custom-event-functions-and-serializability).

#### Example (Elixir)

Same scenario as in the Erlang example.

##### 1\. Start a deigma instance

``` elixir
category = :metrics
{:ok, _pid} = :deigma.start(category)
```

##### 2\. Sample events

``` elixir
category = :metrics
event_type = :http_request

case :deigma.ask(category, event_type) do
    {:sample, sampling_percentage} ->
        YourMetrics.report(:counter, event_type, +1, sampling_percentage)
    {:drop, _sampling_percentage} ->
        :ok
end
```

#### Documentation and Reference

Documentation and reference are hosted on
[HexDocs](https://hexdocs.pm/deigma/).

#### Tested setup

  - Erlang/OTP 22 or higher
  - rebar3

#### Categories

Each `Category` represents an independent group of events and is managed
separately; categories can be launched under your own supervision tree
(using `:child_spec/1` or `:start_link/1`) as well as under the `deigma`
application (using `:start/1`).

Categories launched under `deigma` can be stopped using `:stop/1`.

#### Event windows

Within the context of each `Category`, each distinct `EventType` will be
handled under dedicated event windows that are owned by independent
processes.

These processes are created on demand as new `EventType` values get
sampled, and stopped after 1000 milliseconds of inactivity.

#### Rate limiting

Each time a new event is reported, the rate limit is applied according
to how many events were sampled so far during the previous 1000
milliseconds; if the limit has been or is about to be exceeded, the
event gets dropped.

The default rate limit is set to 100 `EventType` occurrences per second
within a `Category`. It can be overridden using `:ask` options:

``` erlang
Category = metrics,
EventType = http_request,
MaxRate = 50,

case deigma:ask(Category, EventType, [{max_rate, MaxRate}]) of
    {sample, SamplingPercentage} ->
        your_metrics:report(counter, EventType, +1, SamplingPercentage);
    {drop, _SamplingPercentage} ->
        ok
end.
```

#### Custom event functions and serializability

The function invoked upon an event getting registered, within an event
window, can be customized.

This is useful if you need serializability when handling sampling
decisions and percentages, at the expense of increasing the risk of the
event window becoming a performance bottleneck.

``` erlang
Category = metrics,
EventType = http_request,

deigma:ask(
    Category, EventType,
    fun (Timestamp, sample, SamplingPercentage) ->
            your_metrics:report(counter, EventType, +1, SamplingPercentage);
        (_Timestamp, drop, _SamplingPercentage) ->
            ok
    end).
```

  - `Timestamp` is the [monotonic
    timestamp](http://erlang.org/doc/man/erlang.html#monotonic_time-0),
    in native units, at which the event was registered

In this scenario, whatever your function returns (or throws) will be
what `deigma:ask` returns (or throws.)

#### License

MIT License

Copyright (c) 2018-2021 Guilherme Andrade

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

