% Copyright (c) 2016, Chris Barrick
%
% Permission to use, copy, modify, and/or distribute this software for any
% purpose with or without fee is hereby granted, provided that the above
% copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% <module> Templated meta-predicates
%
% This library provides a templating system to ease the development of
% meta-predicates and a handful of basic meta-predicates based on this system.
%
% Templating is implemented by the predicate, replace/3, which takes a template
% term and substitutes arguments for placeholders. Placeholder terms are of the
% form $K and are replaced by the Kth argument.
%
% For example:
% ```
% ?- replace(foo($1, $2), [bar, baz($1)], X).
% X = foo(bar, baz(bar)).
% ```
%
% The rest of the library is dedicated to meta-predicates that take advantage of
% templates. The most basic is call_tmpl/2 which takes a template and arguments
% and calls the goal resulting from the instantiation of the template. The most
% useful may be for/2, which maps a template goal over a list.
%
% For example, to assert that a collection of lists all have length 2:
% ```
% ?- for([X,Y,Z], length($1, 2)).
% X = [_A, _B],
% Y = [_C, _D],
% Z = [_E, _F].
% ```
%
% Variables in the template are replaced for each call. Variables
% and placeholders can be escaped from replacement with \/1.
%
% For example, to assert that a collection of terms all have the same functor:
% ```
% ?- for([foo, foo(_), foo(_,_)], functor($1,\F,_)).
% F = foo.
% ```
%
% Like for/2, many of the meta-predicates loop over collections. In addition to
% lists, all looping predicates accept dicts (in SWI Prolog) and goals which,
% when called with an additional variable argument, bind that argument to a
% list (or dict). The included predicates range/3 and range/4 work with this
% feature to simplify looping over lists of ordered numbers.

:- module(meta, [
	replace/3,
	call_tmpl/2,
	once_tmpl/2,
	ignore_tmpl/2,
	for/2,
	map/2,
	fold/4,
	scan/4,
	foldp/4,
	mapp/3,
	iterable/3,
	iterables/3,
	range/3,
	range/4
]).

% Templating
% -------------------------

%% replace(+Template, +Arguments, -Term) is det
% Term is like Template except all subterms of the form $K in Template are
% replaced by the Kth term in Arguments. In SWI Prolog, Arguments may be a dict.
% If K is not an index or key of Arguments, the term is not replaced.
% Arguments my also include $K terms which are replaced recursively. Variables
% are replaced by new ones. To prevent a replacement, escape the term with \/1.
replace(Template, Args, Term) :-
	replace(Template, Args, Term, _Vars).


%% replace(+Template, +Arguments, -Term, --Vars) is det
% Implementation of replace/3. Vars is an open list used to store pairs mapping
% the original variables in Template to the replacement variables in Term. See
% replace_vars for details on how the list is maintained.

% Variables are substituted, but references to the same variable must be
% substituted by the same replacement. Must be first clause.
replace(X, _, Y, Vars) :-
	var(X),
	replace_vars(X, Vars, Y),
	!.

% To replace a list, replace its elements. This is the critical path since
% compound terms are converted to lists for processing.
replace([X|Template], Arguments, [Y|Term], Vars) :-
	!,
	replace(X, Arguments, Y, Vars),
	replace(Template, Arguments, Term, Vars).

% The functor \/1 is used to escape terms from replacement.
replace(\X, _, X, _) :- !.

% A placeholder term $K is replaced by the Kth argument. If the result is a
% non-variable term, it may contain placeholders, so recursivly replace it.
replace($(N), Arguments, V, Vars) :-
	integer(N),
	nth1(N, Arguments, V0),
	!,
	(nonvar(V0) ->
		replace(V0, Arguments, V, Vars)
	; V = V0).

% In SWI Prolog, Arguments may be a dict. The dict predicates will not be
% defined in other Prologs, so we catch the error and treat it as failure.
replace($(K), Arguments, V, Vars) :-
	catch((
		is_dict(Arguments),
		get_dict(K, Arguments, V0)
	), _, false),
	!,
	(nonvar(V0) ->
		replace(V0, Arguments, V, Vars)
	; V = V0).

% Atomic terms cannot be replaced.
replace(X, _, X, _) :-
	atomic(X),
	!.

% Compound terms are processed as lists.
replace(Template, Arguments, Term, Vars) :-
	Template =.. [F|T0],
	replace(T0, Arguments, T1, Vars),
	Term =.. [F|T1].


%% replace_vars(@Key, ?Pairs, -Value) is det
% Gets a Key-Value pair from Pairs. The Key is tested for strict equality,
% allowing keys to be variables. If Pairs is a variable or an open list and the
% Key is not found, a new Key-Value pair is inserted where Value is a new
% variable. The indended use of this predicate is to maintain the mapping of
% variable replacements.
replace_vars(Key, Pairs, Value)   :- var(Pairs), !, Pairs=[Key-Value|_].
replace_vars(Key, [X-Y|_], Value) :- Key==X, !, Value=Y.
replace_vars(Key, [_|T], Value)   :- !, replace_vars(Key, T, Value).


% Calling
% -------------------------

%% call_tmpl(:Template, +Arguments) is nondet
% Substitutes Arguments into Template with replace/3 and calls the result.
:- meta_predicate call_tmpl(0,+).
call_tmpl(Template, Arguments) :-
	replace(Template, Arguments, Goal),
	call(Goal).


%% once_tmpl(:Template, +Arguments) is semidet
% Substitutes Arguments into Template with replace/3 and calls the result once,
% cutting any leftover choice-points.
:- meta_predicate once_tmpl(0,+).
once_tmpl(Template, Arguments) :-
	replace(Template, Arguments, Goal),
	once(Goal).


%% ignore_tmpl(:Template, +Arguments) is det
% Substitutes Arguments into Template with replace/3 and calls the result once,
% cutting any leftover choice-points and succeeding regardless of whether the
% subgoal succeeds.
:- meta_predicate ignore_tmpl(0,+).
ignore_tmpl(Template, Arguments) :-
	replace(Template, Arguments, Goal),
	call(Goal),
	!.
ignore_tmpl(_,_).


% Looping
% -------------------------

%% for(+Iter, :Template)
% Sugar for map([Iter], Template).
:- meta_predicate for(+,0).
for(I, T) :- map([I], T).


%% map(+Iters, :Template)
% Maps the Template goal over the Iters. Terms of the form $N in the Template
% are replaced by the value of the Nth iterable.
%
% Analogous to SWI Prolog's maplist.
%
% For example:
% ```
% ?- map([X, [1,2,3], [4,5,6]], $1 is $2 + $3).
% X = [5, 7, 9].
% ```
:- meta_predicate map(+,0).
map(Iters, Template) :-
	iterables(Iters, Heads, Tails),
	!,
	call_tmpl(Template, Heads),
	map(Tails, Template).
map([[]|T],_) :- all_empty(T).


%% fold(-V, +V0, +Iters, :Template)
% Folds some Iters into a single value with a Template goal. In the Template,
% $1 refers to the next value of the accumulator, $2 refers to the current value
% of the accumulator, and $N refers to the current value of the (N-2)th list. V0
% is the initial value of the accumulator and V is its final value.
%
% Analogous to SWI Prolog's foldl.
%
% For example:
% ```
% % compute the sum of the pairwise product of two lists.
% % i.e. V is 1*4 + 2*5 + 3*6.
% ?- fold(V, 0, [[1,2,3],[4,5,6]], $1 is $2 + $3 * $4).
% V = 32.
% ```
:- meta_predicate fold(-,+,+,0).
fold(V, V0, Iters, Template) :-
	iterables(Iters, Heads, Tails),
	!,
	call_tmpl(Template, [V1,V0|Heads]),
	fold(V, V1, Tails, Template).
fold(V, V, [[]|T], _) :- all_empty(T).


%% scan(-Vs, +V0, +Iters, :Template)
% Like fold/4, but record intermediate results.
%
% Analogous to SWI Prolog's scanl.
%
% For example:
% ```
% ?- scan(Vs, 0, [[1,2,3],[4,5,6]], $1 is $2 + $3 * $4).
% Vs = [0, 4, 14, 32].
% ```
:- meta_predicate scan(-,+,+,0).
scan([V0|Vs], V0, Iters, Template) :-
	iterables(Iters, Heads, Tails),
	!,
	call_tmpl(Template, [V1,V0|Heads]),
	scan(Vs, V1, Tails, Template).
scan([Vn], Vn, [[]|T], _) :- all_empty(T).


%% foldp(:Template, -Folded, +Identity, +Tree)
% Folded is the partial reduction of Tree, meaning that the innermost lists of
% Tree are reduced according to the Template. In Template, $1 is refers to the
% next value of the accumulator, $2 refers to the current value of the
% accumulator, and $3 refers to the value being brought into the fold. Identity
% is the initial value of the accumulator for all reductions.
%
% For example:
% ```
% ?- P = ($1 is $2 + $3),
%    V0 = [[[1, 2, 3], [4, 5, 6]], [[7, 8, 9]]],
%    foldp(V1, 0, V0, P),
%    foldp(V2, 0, V1, P),
%    foldp(V3, 0, V2, P).
%
% V1 = [[6, 15], [24]],  % [1,2,3] -> 6. [4,5,6] -> 15. [7,8,9] -> 24.
% V2 = [21, 24],         % [6,15] -> 21. [24] -> 24.
% V3 = 45.               % [21,24] -> 45.
% ```
%
% foldp/4 is easier to read than fold/4 for single list folds:
% ```
% ?- foldp(V, 0, [1,2,3,4,5], $1 is $2 + $3).
% V = 15.
% ```
:- meta_predicate foldp(-,+,+,0).
foldp(Folded, Id, Tree, Template) :-
	map([Tree], is_list($1))
	-> map([Folded, Tree], foldp($1, Id, $2, \Template))
	; fold(Folded, Id, [Tree], Template).


%% mapp(-Mapped, +Tree, :Template)
% Mapped is the result of mapping the leaf nodes of a Tree according to the
% Template. In the Template, $1 refers to the mapped node, and $2 refers to the
% original Node.
%
% For example:
% ```
% ?- mapp(M, [[[1,2,3],[4,5,6]],[[7,8,9]]], $1 is $2 + 1).
% M = [[[2, 3, 4], [5, 6, 7]], [[8, 9, 10]]].
% ```
%
% This predicate is easier to read than map/2 for single list maps:
% ```
% ?- mapp(M, [1,2,3,4,5], $1 is $2 + 1).
% M is [2, 3, 4, 5, 6].
% ```
:- meta_predicate mapp(+,-,0).
mapp(Mapped, Tree, Template) :-
	map([Tree], is_list($1))
	-> map([Mapped, Tree], mapp($1, $2, \Template))
	; map([Mapped, Tree], Template).


%% all_empty(+Lists)
% All lists in Lists are empty.
all_empty([]).
all_empty([[]|T]) :- all_empty(T).


% Iterables
% -------------------------

%% iterable(?Iter, ?H, ?T) is semidet
% Iter is an iterable with the head H and tail T.
% If Iter has only one element, T must be [].
%
% The following are valid iterables:
% - Lists are iterables over their elements. The empty list is not an iterable.
% - Dicts in SWI Prolog are iterables over key-value pairs.
% - Any term which can be called with an additional variable argument that is
%   to be unified to a list containing the iterable's elements.
%
% Custom iterators should be implemented with freeze/2 as lazy lists. For
% example, consider this possible implementation for range/3:
% ```
%
% range(A, Z, [A|T]) :-
%     A < Z, !,
%     freeze(T, (
%         B is A + 1,
%         range(B, Z, T)
%     )).
% range(_, _, []).
% ```
iterable([H|T], H, T) :- !.

iterable(Dict, H, T) :-
	catch((
		is_dict(Dict),
		dict_pairs(Dict, _, Pairs)
	), _, false),
	!,
	iterable(Pairs, H, T).

iterable(Iter, H, T) :-
	callable(Iter),
	!,
	call(Iter, [H|T]).


%% iterables(?Iters, ?Heads, ?Tails) is semidet
% Iters is a collection of 0 or more iterables.
% Heads is a list of their heads.
% Tails is a list of their tails.
iterables([],[],[]).
iterables([Iter|Iters], [H|Heads], [T|Tails]) :-
	iterable(Iter, H, T),
	iterables(Iters, Heads, Tails).


%% range(+A, +Z, -Range) is det
% Range is the list of numbers on the interval [A,Z) with a stride of ±1.
% A may be less than or greater than Z.
% Range is a lazy list, so large ranges are OK in deterministic settings.
% This predicate can be used with iterable/3.
range(A, Z, [A|T]) :- A < Z, !, range_up(A, Z, +1, [A|T]).
range(A, Z, [A|T]) :- Z < A, !, range_down(A, Z, -1, [A|T]).


%% range(+A, +Z, +S, -Range) is det
% Range is the list of numbers on the interval [A,Z) with a stride of S.
% If Z is less than A, S must be negative, otherwise S must be positive.
% Range is a lazy list, so large ranges are OK in deterministic settings.
% This predicate can be used with iterable/3.
range(A, Z, S, [A|T]) :- A < Z, 0 < S, !, range_up(A, Z, S, [A|T]).
range(A, Z, S, [A|T]) :- Z < A, S < 0, !, range_down(A, Z, S, [A|T]).


%% range_up(+A, +Z, +S, -Range) is det
% Implementation of range/4 when A < Z.
% Assumes S is positive.
range_up(A, Z, S, [A|T]) :-
	freeze(T, (
		B is A + S,
		B < Z
		-> range_up(B, Z, S, T)
		; T = []
	)).


%% range_down(+A, +Z, +S, -Range) is det
% Implementation of range/4 when Z < A.
% Assumes S is negative.
range_down(A, Z, S, [A|T]) :-
	freeze(T, (
		B is A + S,
		Z < B
		-> range_down(B, Z, S, T)
		; T = []
	)).
