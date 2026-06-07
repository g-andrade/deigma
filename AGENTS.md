# deigma

Erlang/OTP library that performs **continuous event sampling** for Erlang/OTP
and Elixir. Each named **category** samples reported events within continuous
one-second windows, steadily adjusting the sampling percentage so the events
that seep through stay representative while honouring a per-event-type rate
limit. The sampling percentage is returned alongside each decision so downstream
consumers can reason about the original population.

## Build, test, check

```bash
make compile         # rebar3 compile
make test            # eunit + CT + coverage
make check           # check-fast + check-slow
make check-fast      # format check (erlfmt) + xref + dead code (hank) + lint (elvis)
make check-slow      # dialyzer
make format          # auto-format sources with erlfmt
make eunit           # unit tests only
make ct              # common_test suites + coverage
make dialyzer        # type analysis
make doc             # download ex_doc escript -> tmp/, render EEP-48 docs to doc/
make shell           # rebar3 shell with the app started
```

All checks run sequentially (`.NOTPARALLEL`). CI runs `make check-fast`,
`make test` and `make check-slow` on OTP 24-29 (ubuntu-22.04).

## Compiler flags

`warn_export_vars`, `warn_missing_spec`, `warn_unused_import` and
`warnings_as_errors` are always on — every exported function needs a `-spec`.
The `test` and `shell` profiles relax `warn_missing_spec`/`warnings_as_errors`.

## Architecture

```
deigma_sup                  (simple_one_for_one)
└── deigma                   per-category supervisor (rest_for_one), the public API module
    ├── deigma_proc_reg          gen_server: event-type -> event-window pid registry (ETS + monitors)
    └── deigma_event_window_sup  supervisor (simple_one_for_one)
        └── deigma_event_window  one process per (category, event type); the core sampler
```

- `deigma` is the public API **and** the per-category supervisor. `start/1`
  launches a category under the `deigma` application (via `deigma_sup`);
  `start_link/1` / `child_spec/1` launch one under your own supervision tree.
- `deigma_event_window` is **not** a gen_server: it's a manual `proc_lib` + `sys`
  loop. One is spawned on demand per distinct `EventType` within a category,
  keeps a one-second sliding window of `{timestamp, decision}` events in a
  `queue`, and stops after 1000 ms of inactivity. `ask/4` talks to it over a
  monitor-guarded message exchange and can run a caller-supplied `EventFun`
  inside the window process for serialisation.
- `deigma_proc_reg` maps each event type to its window pid, cleaning up via
  process monitors.

### Key modules

| Module | Role |
|---|---|
| `deigma` | Public API + per-category supervisor: `start_link/1`, `child_spec/1`, `start/1`, `stop/1`, `ask/2,3,4`; owns the `ask_opt/0` type |
| `deigma_event_window` | Per-(category, event-type) sampler process: sliding window, rate limiting, custom event funcs |
| `deigma_proc_reg` | Maps event types to window pids within a category |
| `deigma_event_window_sup` | Dynamic supervisor for event-window processes |
| `deigma_sup` | Top-level dynamic supervisor for application-managed categories |
| `deigma_app` | OTP application callback |
| `deigma_util` | Small helpers (`proc_name/2`, `dialyzer_opaque_term/1`) |

## Code conventions

- Module names follow `deigma_<subsystem>[_<role>].erl`.
- Code is formatted with `erlfmt`; run `make format` before committing. The bulk
  reformat commit is listed in `.git-blame-ignore-revs`.
- Documentation is **EEP-48 native**: `-moduledoc`/`-doc` attributes, each
  guarded by `-ifdef(E48). ... -endif.` (the `E48` macro is defined only on
  OTP 27+ via `rebar.config`). `make doc` runs `rebar3 edoc` (using the
  top-level `edoc_opts` chunk doclet) and renders the chunks with ex_doc.
  Private modules and internal functions are hidden with `-moduledoc false` /
  `-doc false` — **not** legacy `%% @private` comments, which ex_doc does not
  honor (a documented module lists every exported function unless it carries a
  `-doc false`). There are no `@private` tags left in `src/`.
- Lint/analysis exceptions are documented inline:
  - `elvis.config`: lowercase function-style time-span macros, internal-only
    types (`export_used_types` disabled), and the monitor-guarded `receive` in
    `deigma_event_window` (`no_receive_without_timeout`).
  - `rebar.config` hank `ignore`: the OTP-mandated `sys` callbacks in
    `deigma_event_window` and `default_ask_fun/3`'s contract-fixed arity.
  - `deigma_util:dialyzer_opaque_term/1` is an identity function that launders a
    value to `term()`. `child_spec/1` routes its map through it so its
    deliberately broad `supervisor:child_spec()` spec is not flagged as an
    `underspec` (deigma never consumes the spec itself, so dialyzer would
    otherwise infer an over-narrow map type).

## Tests

`test/deigma_SUITE.erl` is the single Common Test suite (a parallel group of
`*_test` cases discovered via `module_info`). It exercises sampling decisions and
rates against an independent reference computation, plus custom/ crashing event
functions (using eunit's `assertThrow`/`assertError`/`assertExit`, not `catch`,
which is deprecated on OTP 29).

## OTP version support

Supported OTP 24-29 (the declared `minimum_otp_vsn` is lower, but 24+ is what's
tested). `rebar.config.script` removes the `erlfmt`, `rebar3_hank` and
`rebar3_lint` plugins on OTP ≤ 25 (incompatible there), drops `erlfmt` on
OTP ≤ 26 (it chokes on `-doc` triple-quoted strings), and works around
`rebar3_hank` on OTP 29.

## Releasing

`make publish` runs `rebar3 hex publish --doc-dir=doc` (builds docs first).
Versioning follows SemVer; history is in `CHANGELOG.md` (Keep a Changelog).
