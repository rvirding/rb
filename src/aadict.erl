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

%% File    : aadict.erl
%% Author  : Robert Virding
%% Purpose : Key-Value dictionary as an Arne Andersson tree.

-module(aadict).

%% Standard interface.
-export([new/0,is_key/2,to_list/1,from_list/1,size/1]).
-export([fetch/2,find/2,fetch_keys/1,erase/2]).
-export([store/3,append/3,append_list/3]).
-export([update_val/3,update/3,update/4,update_counter/3]).
-export([fold/3,map/2,filter/2,merge/3]).

%% Extended interface
-export([foreach/2,all/2,any/2,iter/3,itera/3]).

%% Deprecated interface.
-export([dict_to_list/1,list_to_dict/1]).
-deprecated([{dict_to_list,1},{list_to_dict,1}]).

-ifdef(DEBUG).
-export([check/1,erase_check/2,check_depth/1,td/0]).
%%-compile(export_all).
-endif.

%% -compile([export_all]).

%% The algorithms here are taken directly the paper "Balanced search
%% trees made simple" by Arne Andersson, in Proc. Workshop on
%% Algorithms and Data Structures, pages 60--71. Springer Verlag,
%% 1993. AA-trees are very close to RB-trees but the alogorithms are
%% much simpler.
%%
%% The following data structures are used to build the AA-dict:
%%
%% {Level,Left,Key,Val,Right}
%% empty

-type aadict() :: 'empty' | {integer(),any(),any(),any(),any()}.

-define(LVL(N), element(1, N)).
-define(LEFT(N), element(2, N)).
-define(KEY(N), element(3, N)).
-define(VAL(N), element(4, N)).
-define(RIGHT(N), element(5, N)).

-spec new() -> 'empty'.

%% new() -> Dict.

new() -> empty.

-spec is_key(any(), aadict()) -> bool().

%% is_key(Key, Dict) -> true | false.

is_key(_, empty) -> false;
is_key(K, {_,Left,K1,_,_}) when K < K1 ->
    is_key(K, Left);
is_key(K, {_,_,K1,_,Right}) when K > K1 ->
    is_key(K, Right);
is_key(_, {_,_,_,_,_}) -> true.

-spec to_list(aadict()) -> list({any(), any()}).

%% to_list(Dict) -> [{Key,Value}].

to_list(T) -> to_list(T, []).

to_list(empty, List) -> List;
to_list({_,A,Xk,Xv,B}, List) ->
    to_list(A, [{Xk,Xv}|to_list(B, List)]).

-spec from_list(list({any(), any()})) -> aadict().

%% from_list([{Key,Value}]) -> Dict.

from_list(L) ->
    lists:foldl(fun ({K,V}, D) -> store(K, V, D) end, new(), L).

-spec size(aadict()) -> non_neg_integer().

%% size(Dict) -> int().

size(T) -> size1(T).

size1(empty) -> 0;
size1({_,L,_,_,R}) ->
    size1(L) + size1(R) + 1.

-spec fetch(any(), aadict()) -> any().

%% fetch(Key, Dict) -> Value.

fetch(K, {_,Left,K1,_,_}) when K < K1 ->
    fetch(K, Left);
fetch(K, {_,_,K1,_,Right}) when K > K1 ->
    fetch(K, Right);
fetch(_, {_,_,_,Val,_}) -> Val.

-spec find(any(), aadict()) -> {'ok', any()} | 'error'.

%% find(Key, Dict) -> {ok,Value} | error.

find(_, empty) -> error;
find(K, {_,Left,K1,_,_}) when K < K1 ->
    find(K, Left);
find(K, {_,_,K1,_,Right}) when K > K1 ->
    find(K, Right);
find(_, {_,_,_,Val,_}) -> {ok,Val}.

-spec fetch_keys(aadict()) -> list(any()).

%% fetch_keys(Dict) -> [Key].

fetch_keys(T) -> fetch_keys(T, []).

fetch_keys(empty, Tail) -> Tail;
fetch_keys({_,L,K,_,R}, Tail) ->
    fetch_keys(L, [K|fetch_keys(R, Tail)]).

-spec store(any(), any(), aadict()) -> aadict().

%% store(Key, Val, Dict) -> Dict.

store(K, V, T) ->
    store_aux(K, V, T).

store_aux(K, V, empty) -> {1,empty,K,V,empty};
store_aux(K, V, {L,A,Xk,Xv,B}) ->
    if K < Xk ->				%Go down the left
	    New = {L,store_aux(K, V, A),Xk,Xv,B},
	    split(skew(New));
       K > Xk ->				%Go down the right
	    New = {L,A,Xk,Xv,store_aux(K, V, B)},
%%	    split(skew(New));
	    split(New);				%No need to skew here
       true -> {L,A,Xk,V,B}			%No change in the tree
    end.

-spec append(any(), any(), aadict()) -> aadict().
-spec append_list(any(), list(any()), aadict()) -> aadict().

%% append(Key, Val, Dict) -> Dict.
%% append_list(Key, [Val], Dict) -> Dict.

append(K, V, T) -> append_aux(K, [V], T).

append_list(K, V, T) -> append_aux(K, V, T).

append_aux(K, V, empty) -> {1,empty,K,V,empty};
append_aux(K, V, {L,A,Xk,Xv,B}) when K < Xk ->
    New = {L,append_aux(K, V, A),Xk,Xv,B},
    split(skew(New));
append_aux(K, V, {L,A,Xk,Xv,B}) when K > Xk ->
    New = {L,A,Xk,Xv,append_aux(K, V, B)},
    split(New);					%No need to skew here
append_aux(K, V, {L,A,_,Xv,B}) -> {L,A,K,Xv ++ V,B}.

-spec update_val(any(), any(), aadict()) -> aadict().

%% update_val(Key, Val, Dict) -> Dict.

update_val(K, V, {L,A,Xk,Xv,B}) when K < Xk ->
    {L,update_val(K, V, A),Xk,Xv,B};
update_val(K, V, {L,A,Xk,Xv,B}) when K > Xk ->
    {L,A,Xk,Xv,update_val(K, V, B)};
update_val(_, V, {L,A,Xk,_,B}) ->
    {L,A,Xk,V,B}.

-spec update(any(), fun((any()) -> any()), aadict()) -> aadict().

%% update(Key, Fun, Dict) -> Dict.

update(K, F, {L,A,Xk,Xv,B}) when K < Xk ->
    {L,update(K, F, A),Xk,Xv,B};
update(K, F, {L,A,Xk,Xv,B}) when K > Xk ->
    {L,A,Xk,Xv,update(K, F, B)};
update(_, F, {L,A,Xk,Xv,B}) ->
    {L,A,Xk,F(Xv),B}.

-spec update(any(), fun((any()) -> any()), any(), aadict()) -> aadict().

%% update(Key, Fun, Init, Dict) -> Dict.

update(K, F, I, T) -> update_aux(K, F, I, T).

update_aux(K, _, I, empty) -> {1,empty,K,I,empty};
update_aux(K, F, I, {L,A,Xk,Xv,B}) when K < Xk ->
    New = {L,update_aux(K, F, I, A),Xk,Xv,B},
    split(skew(New));
update_aux(K, F, I, {L,A,Xk,Xv,B}) when K > Xk ->
    New = {L,A,Xk,Xv,update_aux(K, F, I, B)},
    split(New);					%No need to skew here
update_aux(_, F, _, {L,A,Xk,Xv,B}) -> {L,A,Xk,F(Xv),B}.

-spec update_counter(any(), number(), aadict()) -> aadict().

%% update_counter(Key, Incr, Dict) -> Dict.

update_counter(K, I, T) -> update_counter_aux(K, I, T).

update_counter_aux(K, I, empty) -> {1,empty,K,I,empty};
update_counter_aux(K, I, {L,A,Xk,Xv,B}) when K < Xk ->
    New = {L,update_counter_aux(K, I, A),Xk,Xv,B},
    split(skew(New));
update_counter_aux(K, I, {L,A,Xk,Xv,B}) when K > Xk ->
    New = {L,A,Xk,Xv,update_counter_aux(K, I, B)},
    split(New);					%No need to skew here
update_counter_aux(_, I, {L,A,Xk,Xv,B}) -> {L,A,Xk,Xv+I,B}.

-spec erase(any(), aadict()) -> aadict().

%% erase(Key, Dict) -> Dict.

erase(K, T) -> {T1,_} = erase_aux(K, T), T1.	       

%% erase_aux(Key, Node) -> {Node,Level}.

erase_aux(_, empty) -> {empty,0};		%No marching node
erase_aux(K, {L,A,Xk,Xv,B}) when K < Xk ->	%Go down the left
    %%io:fwrite("e1: ~p\n", [Node]),
    {A1,La} = erase_aux(K, A),
    New = rebalance_left(La, L, {L,A1,Xk,Xv,B}),
    {New,?LVL(New)};
erase_aux(K, {L,A,Xk,Xv,B}) when K > Xk ->	%Go down the right
    %%io:fwrite("e2: ~p\n", [Node]),
    {B1,Lb} = erase_aux(K, B),
    New = rebalance_right(Lb, L, {L,A,Xk,Xv,B1}),
    {New,?LVL(New)};
%% Found the right node.
erase_aux(_, {_,empty,_,_,empty}) -> {empty,0};
erase_aux(_, {_,empty,_,_,B}) -> {B,?LVL(B)};
erase_aux(_, {L,A,_,_,B}) ->
    %%io:fwrite("e5: ~p\n", [Node]),
    {B1,{Mk,Mv},Lb} = erase_min(B),		%Use the next largest element
    New = rebalance_right(Lb, L, {L,A,Mk,Mv,B1}),
    {New,?LVL(New)}.

%% erase_min(Node) -> {Node,Min,Level}.
%%  Delete and return the minimum node in the tree. The tree is
%%  rebalanced after deletion.

erase_min(N) ->
    %%io:fwrite("em: ~p\n", [N]),
    R = erase_min1(N),
    %%io:fwrite("==> ~p\n", [R]),
    R.

erase_min1({_,empty,Xk,Xv,empty}) ->
    {empty,{Xk,Xv},0};
erase_min1({_,empty,Xk,Xv,{L2,_,_,_,_}=B}) ->
    {B,{Xk,Xv},L2};
erase_min1({L,A,Xk,Xv,B}) ->
    {A1,Min,La} = erase_min(A),
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

-spec fold(fun((any(), any(), any()) -> any()), any(), aadict()) -> any().

%% fold(Fun, Acc, Dict) -> Acc.
%%  Fold Fun over Dict starting with value Acc.

fold(_, Acc, empty) -> Acc;
fold(F, Acc, {_,A,Xk,Xv,B}) ->
    fold(F, F(Xk, Xv, fold(F, Acc, B)), A).

-spec map(fun((any(), any()) -> any()), aadict()) -> aadict().

%% map(Fun, Dict) -> Dict.

map(_, empty) -> empty;
map(F, {L,A,Xk,Xv,B}) ->
    {L,map(F,A),Xk,F(Xk, Xv),map(F, B)}.

-spec filter(fun((any(), any()) -> bool()), aadict()) -> aadict().

%% filter(Fun, Dict) -> Dict.

filter(F, T) -> filter(F, T, new()).

filter(_, empty, New) -> New;
filter(F, {_,A,Xk,Xv,B}, New0) ->
    New1 = filter(F, A, New0),
    New2 = case F(Xk, Xv) of
	       true -> store(Xk, Xv, New1);
	       false -> New1
    end,
    filter(F, B, New2).

%% merge(Fun, Dict, Dict) -> Dict.

-spec merge(fun((any(), any(), any()) -> any()), aadict(), aadict()) ->
    aadict().

merge(F, D1, D2) ->
    fold(fun (K, V2, D) ->
		 update(K, fun(V1) -> F(K, V1, V2) end, V2, D)
	 end, D1, D2).				   

%% Extended interface

-spec foreach(fun((any(), any()) -> any()), aadict()) -> ok.

%% foreach(Fun, Dict) -> ok.
%%  Apply Fun to each element in Dict.

foreach(_, empty) -> ok;
foreach(F, {_,A,Xk,Xv,B}) ->
    %% Do it left to right, even if this is not specified.
    foreach(F, A),
    F(Xk, Xv),
    foreach(F, B).

-spec all(fun((any(), any()) -> bool()), aadict()) -> bool().

%% all(Pred, Dict) -> bool().

all(Pred, Dict) when is_function(Pred, 2) -> all_aux(Pred, Dict).

all_aux(_, empty) -> true;
all_aux(Pred, {_,A,Xk,Xv,B}) ->
    Pred(Xk, Xv) andalso all_aux(Pred, A) andalso all_aux(Pred, B).

%% all2(P, D) when is_function(P, 2) -> all2(P, D, []).

%% all2(_, empty, []) -> true;
%% all2(P, empty, [D|Rest]) ->
%%     all2(P, D, Rest);
%% all2(P, {_,A,Xk,Xv,B}, Rest) ->
%%     case P(Xk, Xv) of
%% 	true -> all2(P, A, [B|Rest]);
%% 	false -> false
%%     end.

-spec any(fun((any(), any()) -> bool()), aadict()) -> bool().

%% any(Pred, Dict) -> bool().

any(Pred, Dict) when is_function(Pred, 2) -> any_aux(Pred, Dict).

any_aux(_, empty) -> false;
any_aux(Pred, {_,A,Xk,Xv,B}) ->
    Pred(Xk, Xv) orelse any_aux(Pred, A) orelse any_aux(Pred, B).

-spec iter(fun((any(), any(), fun(() -> any())) -> any()),
           any(),
           aadict()) -> any().

%% iter(Fun, Default, Dict) -> any().

iter(_, D, empty) -> D;
iter(F, D, {_,empty,Xk,Xv,empty}) ->
    F(Xk, Xv, fun() -> D end);
iter(F, D, {_,A,Xk,Xv,empty}) ->
    F(Xk, Xv, fun() -> iter(F, D, A) end);
iter(F, D, {_,empty,Xk,Xv,B}) ->
    F(Xk, Xv, fun() -> iter(F, D, B) end);
iter(F, D, {_,A,Xk,Xv,B}) ->
    F(Xk, Xv, fun() ->
        iter(F, D, fun() -> iter(F, D, B) end, A)
    end).

iter(F, _, I, {_,empty,Xk,Xv,empty}) ->
    F(Xk, Xv, I);
iter(F, D, I, {_,empty,Xk,Xv,B}) ->
    F(Xk, Xv, fun() -> iter(F, D, I, B) end);
iter(F, D, I, {_,A,Xk,Xv,empty}) ->
    F(Xk, Xv, fun() -> iter(F, D, I, A) end);
iter(F, D, I, {_,A,Xk,Xv,B}) ->
    F(Xk, Xv, fun() ->
        iter(F, D, fun() -> iter(F, D, I, B) end, A)
    end).

-spec itera(fun((any(), any(), any(), fun((any()) -> any())) -> any()),
            any(),
            aadict()) -> any().

%% itera(Fun, Acc, Dict) -> any().

itera(_, Acc, empty) ->
    Acc;
itera(F, Acc, {_,empty,Xk,Xv,empty}) ->
    F(Xk, Xv, Acc, fun(V) -> V end);
itera(F, Acc, {_,A,Xk,Xv,empty}) ->
    F(Xk, Xv, Acc, fun(V) -> itera(F, V, A) end);
itera(F, Acc, {_,empty,Xk,Xv,B}) ->
    F(Xk, Xv, Acc, fun(V) -> itera(F, V, B) end);
itera(F, Acc, {_,A,Xk,Xv,B}) ->
    F(Xk, Xv, Acc, fun(V1) ->
        itera(F, V1, fun(V2) -> itera(F, V2, B) end, A)
    end).
itera(F, Acc, I, {_,empty,Xk,Xv,empty}) ->
    F(Xk, Xv, Acc, I);
itera(F, Acc, I, {_,empty,Xk,Xv,B}) ->
    F(Xk, Xv, Acc, fun(V) -> itera(F, V, I, B) end);
itera(F, Acc, I, {_,A,Xk,Xv,empty}) ->
    F(Xk, Xv, Acc, fun(V) -> itera(F, V, I, A) end);
itera(F, Acc, I, {_,A,Xk,Xv,B}) ->
    F(Xk, Xv, Acc, fun(V1) ->
        itera(F, V1, fun(V2) -> itera(F, V2, I, B) end, A)
    end).

%% Deprecated interface.

%% dict_to_list(Dictionary) -> [{Key,Value}].

dict_to_list(D) -> to_list(D).

%% list_to_dict([{Key,Value}]) -> Dictionary.

list_to_dict(L) -> from_list(L).

-ifdef(DEBUG).

%% Test functions.

erase_check(K, T) ->
    T1 = erase(K, T),
    check(T1),
    T1.

check(empty) -> true;
check({L,A,_,_,B}) when is_integer(L), L > 0 ->
    check_down(A, L),
    check_right(B, L).

check_down({L,A,_,_,B}, Lp) when L == Lp - 1 ->
    check_down(A, L),
    check_right(B, L);
check_down(empty, 1) -> true.

%% Check to the right, only one step at same level!
check_right({L,A,_,_,B}, Lp) when L == Lp ->
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
     {2,{1,empty,a,a,empty},b,b,{1,empty,c,c,empty}},d,d,
     {3,
      {2,
       {1,empty,e,e,empty},
       f,f,
       {2,{1,empty,g,g,empty},h,h,{1,empty,i,i,empty}}},
      j,j,
      {2,{1,empty,k,k,empty},l,l,{1,empty,m,m,empty}}}
    }.
    
-endif.
