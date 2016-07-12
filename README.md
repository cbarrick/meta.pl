# Better meta-predicates for Prolog

This library provides a templating system to ease the development of meta-predicates and a handful of basic meta-predicates based on this system.

Templating is implemented by the predicate, replace/3, which takes a template term and substitutes arguments for placeholders. Placeholder terms are of the form $K and are replaced by the Kth argument. In SWI Prolog, arguments may be provided as a dict.

For example:
```prolog
?- replace(foo($1, $2), [bar, baz($1)], X).
X = foo(bar, baz(bar)).
```

The rest of the library is dedicated to meta-predicates that take advantage of templates. The most basic is call_tmpl/2 which takes a template and arguments and calls the goal resulting from the instantiation of the template. The most useful may be for/2, which maps a template goal over a list.

For example, to assert that a collection of lists all have length 2:
```prolog
?- for([X,Y,Z], length($1, 2)).
X = [_A, _B],
Y = [_C, _D],
Z = [_E, _F].
```

Variables in the template are replaced for each call. Variables and placeholders can be escaped from replacement with \/1.

For example, to assert that a collection of terms all have the same functor:
```prolog
?- for([foo, foo(_), foo(_,_)], functor($1,\F,_)).
F = foo.
```

Like for/2, many of the meta-predicates loop over lists. All looping predicates accept terms of the form range(A,Z,S) and range(A,Z) which behave as lists containing the range [A,Z) discretized with a stride of S. In the latter form, S is taken to be 1 or -1 depending on the order of A and Z. In SWI Prolog, the looping predicates can loop over the pairs of a dict as well. External code can lean on the predicates iterable/3 and interables/3 to loop over lists, dicts, and ranges.


## Why

I've wanted this implementation of map/2 for a while, so I took a free weekend to implement it. The basic infrastructure was generally useful, so I ran with it.

My primary motivation for map/2 was to replace helper predicates that loop over lists with anonymous predicates, and SWI's maplist/2+ wasn't expressive enough to do that.


## API

### Templating

#### replace(+Template, +Arguments, -Term) is det
Term is like Template except all subterms of the form $K in Template are replaced by the Kth term in Arguments. In SWI Prolog, Arguments may be a dict. If K is not an index or key of Arguments, the term is not replaced. Arguments my also include $K terms which are replaced recursively. Variables are replaced by new ones. To prevent a replacement, escape the term with \/1.

### Calling

#### call_tmpl(:Template, +Arguments) is nondet
Substitutes Arguments into Template with replace/3 and calls the result.

#### once_tmpl(:Template, +Arguments) is semidet
Substitutes Arguments into Template with replace/3 and calls the result once, cutting any leftover choice-points.

#### ignore_tmpl(:Template, +Arguments) is det
Substitutes Arguments into Template with replace/3 and calls the result once, cutting any leftover choice-points and succeeding regardless of whether the subgoal succeeds.

### Looping

#### for(+Iter, :Template)
Sugar for map([Iter], Template).

#### map(+Iters, :Template)
Maps the Template goal over the Iters. Terms of the form $N in the Template are replaced by the value of the Nth iterable.

Analogous to SWI Prolog's maplist.

For example:
```prolog
?- map([X, [1,2,3], [4,5,6]], $1 is $2 + $3).
X = [5, 7, 9].
```

#### fold(-V, +V0, +Iters, :Template)
Folds some Iters into a single value with a Template goal. In the Template,$1 refers to the next value of the accumulator, $2 refers to the current value of the accumulator, and $N refers to the current value of the (N-2)th list. V0 is the initial value of the accumulator and V is its final value.

Analogous to SWI Prolog's foldl.

For example:
```prolog
compute the sum of the pairwise product of two lists.
i.e. V is 1*4 + 2*5 + 3*6.
?- fold(V, 0, [[1,2,3],[4,5,6]], $1 is $2 + $3 * $4).
V = 32.
```

#### scan(-Vs, +V0, +Iters, :Template)
Like fold/4, but record intermediate results.

Analogous to SWI Prolog's scanl.

For example:
```prolog
?- scan(Vs, 0, [[1,2,3],[4,5,6]], $1 is $2 + $3 * $4).
Vs = [0, 4, 14, 32].
```

#### foldp(:Template, -Folded, +Identity, +Tree)
Folded is the partial reduction of Tree, meaning that the innermost lists of Tree are reduced according to the Template. In Template, $1 is refers to the next value of the accumulator, $2 refers to the current value of the accumulator, and $3 refers to the value being brought into the fold. Identity is the initial value of the accumulator for all reductions.

For example:
```prolog
?- P = ($1 is $2 + $3),
	V0 = [[[1, 2, 3], [4, 5, 6]], [[7, 8, 9]]],
	foldp(V1, 0, V0, P),
	foldp(V2, 0, V1, P),
	foldp(V3, 0, V2, P).

V1 = [[6, 15], [24]],  % [1,2,3] -> 6. [4,5,6] -> 15. [7,8,9] -> 24.
V2 = [21, 24],         % [6,15] -> 21. [24] -> 24.
V3 = 45.               % [21,24] -> 45.
```

foldp/4 is easier to read than fold/4 for single list folds:
```prolog
?- foldp(V, 0, [1,2,3,4,5], $1 is $2 + $3).
V = 15.
```

#### mapp(-Mapped, +Tree, :Template)
Mapped is the result of mapping the leaf nodes of a Tree according to the Template. In the Template, $1 refers to the mapped node, and $2 refers to the original Node.

For example:
```prolog
?- mapp(M, [[[1,2,3],[4,5,6]],[[7,8,9]]], $1 is $2 + 1).
M = [[[2, 3, 4], [5, 6, 7]], [[8, 9, 10]]].
```

mapp/3 is easier to read than map/2 for single list maps:
```prolog
?- mapp(M, [1,2,3,4,5], $1 is $2 + 1).
M is [2, 3, 4, 5, 6].
```

### Miscellaneous

#### iterable(?Iter, ?H, ?T) is semidet
Iter is an iterable with the head H and tail T. If Iter has only one element, T must be [].

The following are valid iterables:
- Lists are iterables over their elements. The empty list is not an iterable.
- A term of the form range(A,Z,Stride) is an iterable over the half-open interval from A to Z with some possibly negative Stride. A range/3 term is only an iterable if Stride moves A closer to Z.
- Dicts in SWI Prolog are iterables over Key-Value pairs.

Custom iterators can be implemented with freeze/2 as lazy lists:
```prolog
range(Start, Stop, Iter) :-
	freeze(Iter, (
		Start < Stop ->
			Iter = [Start|T],
			Next is Start + 1,
			range(Next, Stop, T)
		; Iter = []
	)).
```

#### iterables(?Iters, ?Heads, ?Tails) is semidet
Iters is a collection of 0 or more iterables. Heads is a list of their heads. Tails is a list of their tails.


## License

Copyright (c) 2016, Chris Barrick

Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted, provided that the above copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
