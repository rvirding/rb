%% Copyright (c) 2009 Robert Virding. All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

%% File    : aasets.erl
%% Author  : Robert Virding
%% Purpose : Sets as an Arne Andersson tree.

-module(aasets).

%% Standard interface.
-export([new/0,is_set/1,size/1,to_list/1,from_list/1]).
-export([is_element/2,add_element/2,del_element/2]).
-export([union/2,union/1,intersection/2,intersection/1]).
-export([subtract/2,is_subset/2]).
-export([fold/3,filter/2]).

%% Extended interface.
-export([foreach/2,all/2,any/2,partition/2,iter/3,itera/3]).

%% Deprecated interface.

-export([new_set/0,set_to_list/1,list_to_set/1,subset/2]).
-deprecated([{new_set,0},{set_to_list,1},{list_to_set,1},{subset,2}]).

-ifdef(DEBUG).
-export([check/1,del_element_check/2,check_depth/1,td/0]).
%%-compile(export_all).
-endif.

%% The algorithms here are taken directly the paper "Balanced search
%% trees made simple" by Arne Andersson, in Proc. Workshop on
%% Algorithms and Data Structures, pages 60--71. Springer Verlag,
%% 1993. AA-trees are very close to RB-trees but the alogorithms are
%% much simpler.
%%
%% The following data structures are used to build the AA-sets:
%%
%% {Level,Left,Element,Right}
%% empty

-type aasets() :: 'empty' | {integer(),any(),any(),any()}.

-define(LVL(N), element(1, N)).
-define(LEFT(N), element(2, N)).
-define(KEY(N), element(3, N)).
-define(VAL(N), element(4, N)).
-define(RIGHT(N), element(5, N)).

-spec new() -> 'empty'.

%% new() -> Set.

new() -> empty.

-spec is_set(aasets()) -> bool().

%% is_set(Set) -> bool().
%%  Return 'true' if Set is a set of elements, else 'false'.

is_set({_,Left,_,Right}) ->
    is_set(Left) andalso is_set(Right);
is_set(empty) -> true.

-spec size(aasets()) -> non_neg_integer().

%% size(Set) -> int().

size(S) -> size1(S).

size1({_,Left,_,Right}) ->
    size1(Left) + 1 + size1(Right);
size1(empty) -> 0.

-spec to_list(aasets()) -> list(any()).

%% to_list(Dict) -> [Element].

to_list(T) -> to_list(T, []).

to_list(empty, List) -> List;
to_list({_,A,X,B}, List) ->
    to_list(A, [X|to_list(B, List)]).

-spec from_list(list(any())) -> aasets().

%% from_list([{Key,Value}]) -> Dict.

from_list(L) ->
    lists:foldl(fun (E, S) -> add_element(E, S) end, new(), L).

-spec is_element(any(), aasets()) -> bool().

%% is_element(Element, Set) -> true | false.

is_element(_, empty) -> false;
is_element(X, {_,A,Y,_}) when X < Y ->
    is_element(X, A);
is_element(X, {_,_,Y,B}) when X > Y ->
    is_element(X, B);
is_element(_, {_,_,_,_}) -> true.

-spec add_element(any(), aasets()) -> aasets().

%% add_element(Element, Set) -> Set.

add_element(E, T) ->
    add_aux(E, T).

add_aux(E, empty) -> {1,empty,E,empty};
add_aux(E, {L,A,Y,B}=T) ->
    if E < Y ->					%Go down the left
	    New = {L,add_aux(E, A),Y,B},
	    split(skew(New));
       E > Y ->					%Go down the right
	    New = {L,A,Y,add_aux(E, B)},
%%	    split(skew(New));
	    split(New);				%No need to skew here
       true -> T				%No change in the tree
    end.

-spec del_element(any(), aasets()) -> aasets().

%% del_element(Element, Set) -> Set.

del_element(K, T) -> {T1,_} = del_aux(K, T), T1.	       

%% del_aux(Key, Node) -> {Node,Level}.

del_aux(_, empty) -> {empty,0};			%No marching node
del_aux(K, {L,A,Xk,Xv,B}) when K < Xk ->	%Go down the left
    %%io:fwrite("e1: ~p\n", [Node]),
    {A1,La} = del_aux(K, A),
    New = rebalance_left(La, L, {L,A1,Xk,Xv,B}),
    {New,?LVL(New)};
del_aux(K, {L,A,Xk,Xv,B}) when K > Xk ->	%Go down the right
    %%io:fwrite("e2: ~p\n", [Node]),
    {B1,Lb} = del_aux(K, B),
    New = rebalance_right(Lb, L, {L,A,Xk,Xv,B1}),
    {New,?LVL(New)};
%% Found the right node.
del_aux(_, {_,empty,_,_,empty}) -> {empty,0};
del_aux(_, {_,empty,_,_,B}) -> {B,?LVL(B)};
del_aux(_, {L,A,_,_,B}) ->
    %%io:fwrite("e5: ~p\n", [Node]),
    {B1,{Mk,Mv},Lb} = del_min(B),		%Use the next largest element
    New = rebalance_right(Lb, L, {L,A,Mk,Mv,B1}),
    {New,?LVL(New)}.

%% del_min(Node) -> {Node,Min,Level}.
%%  Delete and return the minimum node in the tree. The tree is
%%  rebalanced after deletion.

del_min(N) ->
    %%io:fwrite("em: ~p\n", [N]),
    R = del_min1(N),
    %%io:fwrite("==> ~p\n", [R]),
    R.

del_min1({_,empty,Xk,Xv,empty}) ->
    {empty,{Xk,Xv},0};
del_min1({_,empty,Xk,Xv,{L2,_,_,_,_}=B}) ->
    {B,{Xk,Xv},L2};
del_min1({L,A,Xk,Xv,B}) ->
    {A1,Min,La} = del_min(A),
    New = rebalance_left(La, L, {L,A1,Xk,Xv,B}),
    {New,Min,?LVL(New)}.

%% rebalance_left(SubLevel, OldLevel, NewNode) -> NewNode.
%% rebalance_right(SubLevel, OldLevel, NewNode) -> NewNode.
%%  Rebalance the tree after a node has been deleted in the left/right
%%  sub tree. We assume that both sub-trees are in good shape.

rebalance_left(Lsub, Lold, New0) ->
    L1 = Lold-1,				%Old sub-level
    if Lsub < L1 ->				%Level too low
	    New1 = dec_level(New0),		%Decrease level in node
	    New2 = skew3(New1),
	    New = split2(New2),
	    %%io:fwrite("  > ~p\n", [{New0,New1,New2,New}]),
	    New;
       Lsub > L1 ->				%Level too high
	    New = split2(skew3(New0)),
	    %%io:fwrite(" >> ~p\n", [{New0,New}]),
	    New;
       true -> New0				%Level is ok
    end.

rebalance_right(Lsub, Lold, New0) ->
    L1 = Lold-1,
    if Lsub < L1 ->				%Level too low
	    New1 = dec_level(New0),		%Decrease level in node
	    New2 = skew3(New1),
	    New = split2(New2),
	    %%io:fwrite("  > ~p\n", [{New0,New1,New2,New}]),
	    New;
       Lsub == Lold ->				%Level is ok, rebalance?
	    New = split2(skew3(New0)),
	    %%io:fwrite(" >> ~p\n", [{New0,New}]),
	    New;
       true -> New0				%Level is ok
    end.

skew({L,{L,A,Xk,Xv,B},Yk,Yv,C}) ->
    {L,A,Xk,Xv,{L,B,Yk,Yv,C}};
skew(Node) -> Node.

skew(L, {L,A,Xk,Xv,B}, Yk, Yv, C) ->
    {L,A,Xk,Xv,{L,B,Yk,Yv,C}};
skew(L, A, Xk, Xv, B) -> {L,A,Xk,Xv,B}.

skew2({L,{L,A,Xk,Xv,B},Yk,Yv,C}) ->
    {L,A,Xk,Xv,skew({L,B,Yk,Yv,C})};
skew2({L,A,Xk,Xv,B}) -> {L,A,Xk,Xv,skew(B)}.

skew3({L,{L,A,Xk,Xv,B},Yk,Yv,C}) ->
    {L,A,Xk,Xv,skew2({L,B,Yk,Yv,C})};
skew3({L,A,Xk,Xv,B}) -> {L,A,Xk,Xv,skew2(B)}.

split({L,A,Xk,Xv,{L,B,Yk,Yv,{L,C,Zk,Zv,D}}}) ->
    {L+1,{L,A,Xk,Xv,B},Yk,Yv,{L,C,Zk,Zv,D}};
split(Node) -> Node.

split2({L,A,Xk,Xv,{L,B,Yk,Yv,{L,C,Zk,Zv,D}}}) ->
    {L+1,{L,A,Xk,Xv,B},Yk,Yv,split({L,C,Zk,Zv,D})};
split2(Node) -> Node.

dec_level({L,A,Xk,Xv,{L,B,Yk,Yv,C}}) ->
    L1 = L-1,
    {L1,A,Xk,Xv,{L1,B,Yk,Yv,C}};
dec_level({L,A,Kk,Xv,B}) -> {L-1,A,Kk,Xv,B}.

dec_level(L, A, Xk, Xv, {L,B,Yk,Yv,C}) ->
    L1 = L-1,
    {L1,A,Xk,Xv,{L1,B,Yk,Yv,C}};
dec_level(L, A, Kk, Xv, B) -> {L-1,A,Kk,Xv,B}.

-spec union(aasets(), aasets()) -> aasets().

%% union(Set1, Set2) -> Set.
%%  Return the union of Set1 and Set2.

union(S1, S2) ->
    fold(fun (E, S) -> add_element(E, S) end, S1, S2).

-spec union(list(aasets())) -> aasets().

%% union([Set]) -> Set.
%%  Return the union of the list of sets.

union([S1,S2|Ss]) ->
    union([union(S1, S2)|Ss]);
union([S]) -> S;
union([]) -> new().

-spec intersection(aasets(), aasets()) -> aasets().

%% intersection(Set1, Set2) -> Set.
%%  Return the intersection of Set1 and Set2.

intersection(S1, S2) ->
    filter(fun (E) -> is_element(E, S2) end, S1).

%% intersection([Set]) -> Set.
%%  Return the intersection of the list of sets.

-spec intersection(list(aasets())) -> aasets().

intersection([S1,S2|Ss]) ->
    intersection([intersection(S1, S2)|Ss]);
intersection([S]) -> S.

-spec subtract(aasets(), aasets()) -> aasets().

%% subtract(Set1, Set2) -> Set.
%%  Return all and only the elements of Set1 which are not also in
%%  Set2.

subtract(S1, S2) ->
    filter(fun (E) -> not is_element(E, S2) end, S1).

-spec is_subset(aasets(), aasets()) -> bool().

%% is_subset(Set1, Set2) -> bool().
%%  Return 'true' when every element of Set1 is also a member of
%%  Set2, else 'false'.

is_subset(S1, S2) ->
    all(fun (E) -> is_element(E, S2) end, S1).

-spec fold(fun((any(), any()) -> any()), any(), aasets()) -> any().

%% fold(Fun, Acc, Set) -> Acc.
%%  Fold Fun over Set starting with value Acc.

fold(_, Acc, empty) -> Acc;
fold(F, Acc, {_,A,E,B}) ->
    fold(F, F(E, fold(F, Acc, B)), A).

-spec filter(fun((any()) -> bool()), aasets()) -> aasets().

%% filter(Pred, Set) -> Set.

filter(F, T) -> filter(F, T, new()).

filter(_, empty, New) -> New;
filter(P, {_,A,E,B}, New0) ->
    New1 = filter(P, A, New0),
    New2 = case P(E) of
	       true -> add_element(E, New1);
	       false -> New1
    end,
    filter(P, B, New2).

%% Extended interface

-spec foreach(fun((any()) -> any()), aasets()) -> ok.

%% foreach(Fun, Set) -> ok.
%%  Apply Fun to each element in Set.

foreach(_, empty) -> ok;
foreach(F, {_,A,X,B}) ->
    %% Do it left to right, even if this is not specified.
    foreach(F, A),
    F(X),
    foreach(F, B).

-spec all(fun((any(), any()) -> bool()), aasets()) -> bool().

%% all(Pred, Set) -> bool().

all(Pred, Set) when is_function(Pred, 2) -> all1(Pred, Set).

all1(_, empty) -> true;
all1(Pred, {_,A,X,B}) ->
    Pred(X) andalso all1(Pred, A) andalso all1(Pred, B).

-spec any(fun((any(), any()) -> bool()), aasets()) -> bool().

%% any(Pred, Set) -> bool().

any(Pred, Set) when is_function(Pred, 2) -> any1(Pred, Set).

any1(_, empty) -> false;
any1(Pred, {_,A,X,B}) ->
    Pred(X) orelse any1(Pred, A) orelse any1(Pred, B).

-spec partition(fun((any()) -> any()), aasets()) -> {aasets(), aasets()}.

%% partition(Pred, Set) -> {Set1,Set2}.
%%  Partition Set so Set1 contains all elements for which Pred(E) is true.

partition(P, S) -> partition(P, S, new(), new()).

partition(_, empty, T, F) -> {T,F};
partition(P, {_,A,X,B}, T, F) ->
    {T1,F1} = partition(P, A, T, F),
    case P(X) of
	true -> partition(P, B, add_element(X, T1), F1);
	false -> partition(P, B, T1, add_element(X, F1))
    end.

%% fold(fun (X, {T,F}) ->
%% 	     case P(X) of
%% 		 true -> {add_element(X, T),F};
%% 		 false -> {T,add_element(X, F)}
%% 	     end
%%      end, {new(),new()}, S).

-spec iter(fun((any(), fun(() -> any())) -> any()), any(), aasets()) -> any().

%% iter(Fun, Default, Set) -> any().

iter(_, D, empty) ->
    D;
iter(F, D, {_,empty,X,empty}) ->
    F(X, fun() -> D end);
iter(F, D, {_,A,X,empty}) ->
    F(X, fun() -> iter(F, D, A) end);
iter(F, D, {_,empty,X,B}) ->
    F(X, fun() -> iter(F, D, B) end);
iter(F, D, {_,A,X,B}) ->
    F(X, fun() ->
        iter(F, D, fun() -> iter(F, D, B) end, A)
    end).
iter(F, _, I, {_,empty,X,empty}) ->
    F(X, I);
iter(F, D, I, {_,empty,X,B}) ->
    F(X, fun() -> iter(F, D, I, B) end);
iter(F, D, I, {_,A,X,empty}) ->
    F(X, fun() -> iter(F, D, I, A) end);
iter(F, D, I, {_,A,X,B}) ->
    F(X, fun() ->
        iter(F, D, fun() -> iter(F, D, I, B) end, A)
    end).

-spec itera(fun((any(), any(), fun((any()) -> any())) -> any()),
            any(),
            aasets()) -> any().

%% itera(Fun, Acc, Set) -> any().

itera(_, Acc, empty) ->
    Acc;
itera(F, Acc, {_,empty,X,empty}) ->
    F(X, Acc, fun(V) -> V end);
itera(F, Acc, {_,A,X,empty}) ->
    F(X, Acc, fun(V) -> itera(F, V, A) end);
itera(F, Acc, {_,empty,X,B}) ->
    F(X, Acc, fun(V) -> itera(F, V, B) end);
itera(F, Acc, {_,A,X,B}) ->
    F(X, Acc, fun(V1) ->
        itera(F, V1, fun(V2) -> itera(F, V2, B) end, A)
    end).
itera(F, Acc, I, {_,empty,X,empty}) ->
    F(X, Acc, I);
itera(F, Acc, I, {_,empty,X,B}) ->
    F(X, Acc, fun(V) -> itera(F, V, I, B) end);
itera(F, Acc, I, {_,A,X,empty}) ->
    F(X, Acc, fun(V) -> itera(F, V, I, A) end);
itera(F, Acc, I, {_,A,X,B}) ->
    F(X, Acc, fun(V1) ->
        itera(F, V1, fun(V2) -> itera(F, V2, I, B) end, A)
    end).

%% Deprecated interface.

new_set() -> new().

set_to_list(D) -> to_list(D).

list_to_set(L) -> from_list(L).

subset(S1, S2) -> is_subset(S1, S2).

-ifdef(DEBUG).

%% Test functions.

del_element_check(K, T) ->
    T1 = del_element(K, T),
    check(T1),
    T1.

check(empty) -> true;
check({L,A,_,B}) when is_integer(L), L > 0 ->
    check_down(A, L),
    check_right(B, L).

check_down({L,A,_,B}, Lp) when L == Lp - 1 ->
    check_down(A, L),
    check_right(B, L);
check_down(empty, 1) -> true.

%% Check to the right, only one step at same level!
check_right({L,A,_,B}, Lp) when L == Lp ->
    %% Same level no must go down.
    check_down(A, L),
    check_down(B, L);				%No more at same level
check_right(Node, Lp) ->
    %% Normal node here.
    check_down(Node, Lp).

check_depth(T) -> check_depth(T, 1, orddict:new()).

check_depth(empty, D, Dd) ->
    orddict:update_counter(D, 1, Dd);
check_depth({_,A,_,_,B}, D, Dd0) ->
    Dd1 = orddict:update_counter(D, 1, Dd0),
    Dd2 = check_depth(A, D+1, Dd1),
    check_depth(B, D+1, Dd2).

td() ->
    {3,
     {2,{1,empty,a,empty},b,{1,empty,c,empty}},d,d,
     {3,
      {2,
       {1,empty,e,empty},
       f,
       {2,{1,empty,g,empty},h,{1,empty,i,empty}}},
      j,
      {2,{1,empty,k,empty},l,{1,empty,m,empty}}}
    }.
    
-endif.
