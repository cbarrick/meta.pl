# Better meta-call predicates for Prolog

This library provides a templating system to ease the development of meta-predicates and a handful of basic meta-predicates based on this system.

Templating is implemented by the predicate, replace/3, which takes a template term and subsitutes arguments for placeholders. Placeholder terms are of the form $K and are replaced by the Kth argument. In SWI Prolog, arguments may be provided as a dict.

For example:
```prolog
?- replace(foo($1, $2), [bar, baz($1)], X).
X = foo(bar, baz(bar)).
```

The rest of the library is dedicated to meta-predicates that take advantage of templates. The most basic is call_tmpl/2 which takes a template and arguments and calls the goal resulting from the instantiation of the template. The most useful of these may be for/2, which maps a template goal over a list.

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

Like for/2, most of the meta-predicates loop over lists. All looping constructs accept terms of the form range(A,Z,S) and range(A,Z) which behave as the the range [A,Z) discretized with a stride of S. In the latter form, S is taken to be 1 or -1 depending on the order of A and Z. In SWI Prolog, the looping constructs can loop over the pairs of a dict as well. External code can lean on the looping predicates iterable/3 and interables/3.

## Why

I've wanted this implementation of map/2 for a while, so I took a free weekend to implement it. The basic infrastructure was generally useful, so I ran with it.

My primary motivation for map/2 was to replace helper predicates that loop over lists with anonymous predicates, and SWI's maplist/2+ wasn't expressive enough to do that.

## License

Copyright (c) 2016, Chris Barrick

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
