

# Module bottlenape #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="types"></a>

## Data Types ##




### <a name="type-child_spec">child_spec()</a> ###


<pre><code>
child_spec(Context) = #{id =&gt; {'?MODULE', Context}, start =&gt; {'?MODULE', start_link, [Context, ...]}}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#child_spec-1">child_spec/1</a></td><td>Returns a child specification for instantiating
a particular <code>Context</code> under a supervisor.</td></tr><tr><td valign="top"><a href="#hit-2">hit/2</a></td><td>Reports one more hit for event <code>Id</code> within context <code>Context</code>.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>Creates a standalone <code>Context</code> process, that is,
a bottlenape process that is not part of a supervision tree
and thus has no supervisor.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>Creates a <code>Context</code> process as part of a supervision tree.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="child_spec-1"></a>

### child_spec/1 ###

<pre><code>
child_spec(Context) -&gt; <a href="#type-child_spec">child_spec</a>(Context)
</code></pre>

<ul class="definitions"><li><code>Context = atom()</code></li></ul>

Returns a child specification for instantiating
a particular `Context` under a supervisor.

__See also:__ [start/1](#start-1), [start/2](#start-2).

<a name="hit-2"></a>

### hit/2 ###

<pre><code>
hit(Context, Id) -&gt; {ok, SampleRate} | drop
</code></pre>

<ul class="definitions"><li><code>Context = atom()</code></li><li><code>Id = term()</code></li><li><code>SampleRate = float()</code></li></ul>

Reports one more hit for event `Id` within context `Context`.
Returns `{ok, SampleRate}` if the event was accepted with `0 < SampleRate =< 1` sampling,
or `drop` if the event was rejected.
`Context` must refer to a previously created context using `child_spec/1`,
`start/1` or `start_link/1`.

<a name="start-1"></a>

### start/1 ###

<pre><code>
start(Context) -&gt; {ok, pid()}
</code></pre>

<ul class="definitions"><li><code>Context = atom()</code></li></ul>

Creates a standalone `Context` process, that is,
a bottlenape process that is not part of a supervision tree
and thus has no supervisor.

__See also:__ [child_spec/1](#child_spec-1), [start_link/1](#start_link-1).

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Context) -&gt; {ok, pid()}
</code></pre>

<ul class="definitions"><li><code>Context = atom()</code></li></ul>

Creates a `Context` process as part of a supervision tree.
This function is to be called, directly or indirectly, by the supervisor.
For example, it ensures that the bottlenape process is linked to the supervisor.

__See also:__ [child_spec/1](#child_spec-1), [start/1](#start-1).

