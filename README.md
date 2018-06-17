# deigma

[![](https://img.shields.io/hexpm/v/deigma.svg?style=flat)](https://hex.pm/packages/deigma)
[![](https://travis-ci.org/g-andrade/deigma.png?branch=master)](https://travis-ci.org/g-andrade/deigma)

`deigma` is a library for Erlang/OTP and Elixir that allows you to
sample events within continuous 1 second windows\[1\] based on rate
limits.

The sampling rate is steadily adjusted so that the events that seep
through are representative of what's happening in the system.

\[\*\] As far as the monotonic clock resolution goes.

#### Example

There's heavy duty a web service; we want to report metrics on inbound
http requests to [StatsD](https://github.com/etsy/statsd) service over
UDP while minimising the risk of dropped datagrams due to too many
events.

For this, we can downsample the reported metrics and determine the real
sampling rate using `deigma`.

##### 1\. Start a deigma instance

``` erlang
Category = metrics,
{ok, _Pid} = deigma:start(Category).
```

##### 2\. Sample events based on a maximum rate

``` erlang
Category = metrics,
EventType = http_request,
MaxRate = 100,
case deigma:ask(Category, EventType, [{max_rate, MaxRate}]) of
    {sample, SampleRate} ->
        your_metrics:report(counter, EventType, +1, SampleRate);
    {drop, _SampleRate} ->
        ok
end.
```

  - [`Category`](#categories) must be an atom
  - [`EventType`](#event_windows) can be any term
  - [`MaxRate`](#rate_limiting) is the maximum number of events per
    second we want to sample (defaults to 100)
  - `SampleRate` is a floating point number between 0.0 and 1.0
    representing the percentage of events that were sampled during the
    last 1000 milliseconds, including the event reported just now.

#### Documentation and Reference

Documentation is hosted on [HexDocs](https://hexdocs.pm/deigma/).

#### Tested setup

  - Erlang/OTP 18 or higher
  - rebar3

#### On categories

Each `Category` represents an independent group of events and is managed
separately; categories can be launched under your own supervision tree
(using `:child_spec/1` or `:start_link/1`) as well as under the `deigma`
application (using `:start/1`).

Categories launched under `deigma` can be stopped using `:stop/1`.

#### On event windows

Within the context of each `Category`, each distinct `EventType` will be
handled under dedicated event windows which are owned by independent
processes.

These processes are created on-demand as new `EventType` values get
sampled, and stopped after 1000 milliseconds of inactivity.

#### On rate limits

Each time a new event is reported, `MaxRate` is applied according to how
many events were sampled so far during the previous 1000 milliseconds;
if the limit has been or is about to be exceeded, the event gets
dropped.

#### License

MIT License

Copyright (c) 2018 Guilherme Andrade

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
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

