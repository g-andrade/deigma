# deigma

[![](https://img.shields.io/hexpm/v/deigma.svg?style=flat)](https://hex.pm/packages/deigma)
[![](https://travis-ci.org/g-andrade/deigma.png?branch=master)](https://travis-ci.org/g-andrade/deigma)

### <span id="deigma_-_Event_sampler">deigma - Event sampler</span>

`deigma` is a library for Erlang/OTP and Elixir that allows you to
sample events based on arbitrary rate limits.

The sampling rate is calculated automatically and continuously adjusted
over a one second window so that the events that go through are
representative of what's happening in the system.

#### <span id="Usage">Usage</span>

``` erlang
EventType = inbound_http_request,
MaxPerSecond = 100,

case deigma:report(EventType, MaxPerSecond) of
    {accept, SampleRate} ->
        % your_metrics:report(EventType, SampleRate);
    {drop, _SampleRate} ->
        % ok
end.
```

  - `EventType` is an arbitrary term that categorizes your event
  - `MaxPerSecond` is a ceiling on how many `EventType` occurences
    you're willing to `accept`, per second
  - `SampleRate` represents the percentage of `EventType` occurences
    that were accepted over the last second; it's a floating point
    number between `0.0` and `1.0`

#### <span id="Details">Details</span>

##### <span id="Requirements">Requirements</span>

  - Erlang/OTP 18 or higher
  - rebar3

##### <span id="Documentation">Documentation</span>

Documentation is hosted on [HexDocs](https://hexdocs.pm/deigma/).

#### <span id="License">License</span>

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

