# deigma

[![](https://img.shields.io/hexpm/v/deigma.svg?style=flat)](https://hex.pm/packages/deigma)
[![](https://travis-ci.org/g-andrade/deigma.png?branch=master)](https://travis-ci.org/g-andrade/deigma)

`deigma` is a library for Erlang/OTP and Elixir that allows you to
sample events within continuous 1 second windows\[1\].

The sampling rate is steadily adjusted so that the events that go
through are representative of what's happening in the system.

\[\*\] As far as the resolution of monotonic clock goes.

#### Example

``` erlang
{ok, _} = deigma:start(metrics).
```

``` erlang
case deigma:ask(metrics, http_requests, [{max_rate, 100}]) of
    {accept, SampleRate} ->
        your_metrics:report(http_requests, SampleRate);
    {drop, _SampleRate} ->
        ok
end.
```

#### Details

#### Tested setup

  - Erlang/OTP 18 or higher
  - rebar3

#### Documentation

Documentation is hosted on [HexDocs](https://hexdocs.pm/deigma/).

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

