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

%% File    : rbdict.erl
%% Author  : Robert Virding
%% Purpose : Key-Value dictionary as a Red-Black tree.

-module(rbdict).

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
-export([check/1,erase_check/2,check_depth/1,t/1,r1/0,r2/0]).
-endif.

%% The algorithms here are taken directly from Okasaki and Rbset in
%% ML/Scheme. The interface is compatible with the standard dict
%% interface.
%%
%% The following structures are used to build the RB-dict:
%%
%% {r,Left,Key,Val,Right}
%% {b,Left,Key,Val,Right}
%% empty
%%
%% It is interesting to note that expanding out the first argument of
%% l/rbalance, the colour, in store etc. is actually slower than not
%% doing it. Measured.

-type rbdict() :: 'empty' |
                  {'b', 'empty', any(), any(), 'empty'} |
                  {'b',
                   'empty',
                   any(), any(),
                   {'r', 'empty', any(), any(), 'empty'}} |
                  {'b',
                   {'r', 'empty', any(), any(), 'empty'},
                   any(), any(),
                   'empty'} |
                  {'b',
                   {'r', 'empty', any(), any(), 'empty'},
                   any(), any(),
                   {'r', 'empty', any(), any(), 'empty'}} |
                  {'b',
                   {'r' | 'b', tuple(), any(), any(), tuple()},
                   any(), any(),
                   {'r' | 'b', tuple(), any(), any(), tuple()}}.

-spec new() -> 'empty'.

%% new() -> Dict.

new() -> empty.

-spec is_key(any(), rbdict()) -> bool().

%% is_key(Key, Dict) -> true | false.

is_key(_, empty) -> false;
is_key(K, {_,Left,K1,_,_}) when K < K1 ->
    is_key(K, Left);
is_key(K, {_,_,K1,_,Right}) when K > K1 ->
    is_key(K, Right);
is_key(_, {_,_,_,_,_}) -> true.

-spec to_list(rbdict()) -> list({any(), any()}).

%% to_list(Dict) -> [{Key,Value}].

to_list(T) -> to_list(T, []).

to_list(empty, List) -> List;
to_list({_,A,Xk,Xv,B}, List) ->
    to_list(A, [{Xk,Xv}|to_list(B, List)]).

-spec from_list(list({any(), any()})) -> rbdict().

%% from_list([{Key,Value}]) -> Dict.

from_list(L) ->
    lists:foldl(fun ({K,V}, D) -> store(K, V, D) end, new(), L).

-spec size(rbdict()) -> non_neg_integer().

%% size(Dict) -> int().

size(T) -> size1(T).

size1(empty) -> 0;
size1({_,L,_,_,R}) ->
    size1(L) + size1(R) + 1.

-spec fetch(any(), rbdict()) -> any().

%% fetch(Key, Dict) -> Value.

fetch(K, {_,Left,K1,_,_}) when K < K1 ->
    fetch(K, Left);
fetch(K, {_,_,K1,_,Right}) when K > K1 ->
    fetch(K, Right);
fetch(_, {_,_,_,Val,_}) -> Val.

-spec find(any(), rbdict()) -> {'ok', any()} | 'error'.

%% find(Key, Dict) -> {ok,Value} | error.

find(_, empty) -> error;
find(K, {_,Left,K1,_,_}) when K < K1 ->
    find(K, Left);
find(K, {_,_,K1,_,Right}) when K > K1 ->
    find(K, Right);
find(_, {_,_,_,Val,_}) -> {ok,Val}.

-spec fetch_keys(rbdict()) -> list(any()).

%% fetch_keys(Dict) -> [Key].

fetch_keys(T) -> fetch_keys(T, []).

fetch_keys(empty, Tail) -> Tail;
fetch_keys({_,L,K,_,R}, Tail) ->
    fetch_keys(L, [K|fetch_keys(R, Tail)]).

-spec store(any(), any(), rbdict()) -> rbdict().

%% store(Key, Val, Dict) -> Dict.

store(K, V, T) ->
    {_,L,K1,V1,R} = store_aux(K, V, T),
    {b,L,K1,V1,R}.				%setelement(1, b, T1).

store_aux(K, V, empty) -> {r,empty,K,V,empty};
store_aux(K, V, {C,Left,K1,V1,Right}) when K < K1 ->
    lbalance(C, store_aux(K, V, Left), K1, V1, Right);
store_aux(K, V, {C,Left,K1,V1,Right}) when K > K1 ->
    rbalance(C, Left, K1, V1, store_aux(K, V, Right));
store_aux(K, V, {C,L,_,_,R}) ->
    {C,L,K,V,R}.

%% Expanding out l/rbalance is slower!
%% store_aux(K, V, empty) -> {r,empty,K,V,empty};
%% store_aux(K, V, {r,Left,K1,V1,Right}) ->
%%     if K < K1 -> {r,store_aux(K, V, Left),K1,V1,Right};
%%        K > K1 -> {r,Left,K1,V1,store_aux(K, V, Right)};
%%        true -> {r,Left,K,V,Right}
%%     end;
%% store_aux(K, V, {b,Left,K1,V1,Right}) ->
%%     if K < K1 ->
%% 	    lbalance(store_aux(K, V, Left), K1, V1, Right);
%%        K > K1 ->
%% 	    rbalance(Left, K1, V1, store_aux(K, V, Right));
%%        true -> {b,Left,K,V,Right}
%%     end.

-spec append(any(), any(), rbdict()) -> rbdict().
-spec append_list(any(), list(any()), rbdict()) -> rbdict().

%% append(Key, Val, Dict) -> Dict.
%% append_list(Key, [Val], Dict) -> Dict.

append(K, V, T) ->
    {_,L,K1,V1,R} = append_aux(K, [V], T),
    {b,L,K1,V1,R}.				%setelement(1, b, T1).

append_list(K, V, T) ->
    {_,L,K1,V1,R} = append_aux(K, V, T),
    {b,L,K1,V1,R}.				%setelement(1, b, T1).

append_aux(K, V, empty) -> {r,empty,K,V,empty};
append_aux(K, V, {C,Left,K1,V1,Right}) when K < K1 ->
    lbalance(C, append_aux(K, V, Left), K1, V1, Right);
append_aux(K, V, {C,Left,K1,V1,Right}) when K > K1 ->
    rbalance(C, Left, K1, V1, append_aux(K, V, Right));
append_aux(K, V, {C,L,_,V1,R}) -> {C,L,K,V1 ++ V,R}.

-spec update_val(any(), any(), rbdict()) -> rbdict().

%% update_val(Key, Val, Dict) -> Dict.

update_val(K, V, {RB,A,Xk,Xv,B}) when K < Xk ->
    {RB,update_val(K, V, A),Xk,Xv,B};
update_val(K, V, {RB,A,Xk,Xv,B}) when K > Xk ->
    {RB,A,Xk,Xv,update_val(K, V, B)};
update_val(_, V, {RB,A,Xk,_,B}) ->
    {RB,A,Xk,V,B}.

-spec update(any(), fun((any()) -> any()), rbdict()) -> rbdict().

%% update(Key, Fun, Dict) -> Dict.

update(K, F, {RB,A,Xk,Xv,B}) when K < Xk ->
    {RB,update(K, F, A),Xk,Xv,B};
update(K, F, {RB,A,Xk,Xv,B}) when K > Xk ->
    {RB,A,Xk,Xv,update(K, F, B)};
update(_, F, {RB,A,Xk,Xv,B}) ->
    {RB,A,Xk,F(Xv),B}.

-spec update(any(), fun((any()) -> any()), any(), rbdict()) -> rbdict().

%% update(Key, Fun, Init, Dict) -> Dict.

update(K, F, I, T) ->
    {_,L,K1,V1,R} = update_aux(K, F, I, T),
    {b,L,K1,V1,R}.				%setelement(1, b, T1).

update_aux(K, _, I, empty) -> {r,empty,K,I,empty};
update_aux(K, F, I, {RB,A,Xk,Xv,B}) when K < Xk ->
    lbalance(RB, update_aux(K, F, I, A), Xk, Xv, B);
update_aux(K, F, I, {RB,A,Xk,Xv,B}) when K > Xk ->
    rbalance(RB, A, Xk, Xv, update_aux(K, F, I, B));
update_aux(_, F, _, {RB,A,Xk,Xv,B}) ->
    {RB,A,Xk,F(Xv),B}.

-spec update_counter(any(), number(), rbdict()) -> rbdict().

%% update_counter(Key, Incr, Dict) -> Dict.

update_counter(K, I, T) ->
    {_,L,K1,V1,R} = update_counter_aux(K, I, T),
    {b,L,K1,V1,R}.				%setelement(1, b, T1).

update_counter_aux(K, I, empty) -> {r,empty,K,I,empty};
update_counter_aux(K, I, {RB,A,Xk,Xv,B}) when K < Xk ->
    lbalance(RB, update_counter_aux(K, I, A), Xk, Xv, B);
update_counter_aux(K, I, {RB,A,Xk,Xv,B}) when K > Xk ->
    rbalance(RB, A, Xk, Xv, update_counter_aux(K, I, B));
update_counter_aux(_, I, {RB,A,Xk,Xv,B}) ->
    {RB,A,Xk,Xv+I,B}.

%% lbalance(Colour, Left, Key, Val, Right).
%% rbalance(Colour, Left, Key, Val, Right).
%%  Balance a tree afer (possibly) adding a node to the left/right.

lbalance(b, {r,{r,A,Xk,Xv,B},Yk,Yv,C}, Zk, Zv, D) ->
    {r,{b,A,Xk,Xv,B},Yk,Yv,{b,C,Zk,Zv,D}};
lbalance(b, {r,A,Xk,Xv,{r,B,Yk,Yv,C}}, Zk, Zv, D) ->
    {r,{b,A,Xk,Xv,B},Yk,Yv,{b,C,Zk,Zv,D}};
lbalance(C, A, Xk, Xv, B) -> {C,A,Xk,Xv,B}.

rbalance(b, A, Xk, Xv, {r,{r,B,Yk,Yv,C},Zk,Zv,D}) ->
    {r,{b,A,Xk,Xv,B},Yk,Yv,{b,C,Zk,Zv,D}};
rbalance(b, A, Xk, Xv, {r,B,Yk,Yv,{r,C,Zk,Zv,D}}) ->
    {r,{b,A,Xk,Xv,B},Yk,Yv,{b,C,Zk,Zv,D}};
rbalance(C, A, Xk, Xv, B) -> {C,A,Xk,Xv,B}.

-spec erase(any(), rbdict()) -> rbdict().

%% erase(Key, Dict) -> Dict.

erase(K, T) ->
    {T1,_} = erase_aux(K, T),
    T1.

%% erase_aux(Key, Node) -> {Node,Decreased}.

erase_aux(_, empty) -> {empty,false};
erase_aux(K, {b,A,Xk,Xv,B}) ->
    if K < Xk ->
	    {A1,Dec} = erase_aux(K, A),
	    if  Dec -> unbalright(b, A1, Xk, Xv, B);
		true -> {{b,A1,Xk,Xv,B},false}
	    end;
       K > Xk ->
	    {B1,Dec} = erase_aux(K, B),
	    if  Dec -> unballeft(b, A, Xk, Xv, B1);
		true -> {{b,A,Xk,Xv,B1},false}
	    end;
       true ->
	    case B of
		empty -> blackify(A);
		_ ->
		    {B1,{Mk,Mv},Dec} = erase_min(B),
		    if  Dec -> unballeft(b, A, Mk, Mv, B1);
			true -> {{b,A,Mk,Mv,B1},false}
		    end
	    end
    end;
erase_aux(K, {r,A,Xk,Xv,B}) ->
    if K < Xk ->
	    {A1,Dec} = erase_aux(K, A),
	    if  Dec -> unbalright(r, A1, Xk, Xv, B);
		true -> {{r,A1,Xk,Xv,B},false}
	    end;
       K > Xk ->
	    {B1,Dec} = erase_aux(K, B),
	    if  Dec -> unballeft(r, A, Xk, Xv, B1);
		true -> {{r,A,Xk,Xv,B1},false}
	    end;
       true ->
	    case B of
		empty -> {A,false};
		_ ->
		    {B1,{Mk,Mv},Dec} = erase_min(B),
		    if  Dec -> unballeft(r, A, Mk, Mv, B1);
			true -> {{r,A,Mk,Mv,B1},false}
		    end
	    end
    end.

%% erase_min(Node) -> {Node,{NodeKey,NodeVal},Decreased}.

erase_min({b,empty,Xk,Xv,empty}) ->
    {empty,{Xk,Xv},true};
erase_min({b,empty,Xk,Xv,{r,A,Yk,Yv,B}}) ->
    {{b,A,Yk,Yv,B},{Xk,Xv},false};
erase_min({b,empty,_,_,{b,_,_,_,_}}) -> exit(boom);
erase_min({r,empty,Xk,Xv,A}) ->
    {A,{Xk,Xv},false};
%% Rec from left
erase_min({b,A,Xk,Xv,B}) ->
    {A1,Min,Dec} = erase_min(A),
    if Dec ->
	    {T,Dec1} = unbalright(b, A1, Xk, Xv, B),
	    {T,Min,Dec1};
       true -> {{b,A1,Xk,Xv,B},Min,false}
    end;
erase_min({r,A,Xk,Xv,B}) ->
    {A1,Min,Dec} = erase_min(A),
    if Dec ->
	    {T,Dec1} = unbalright(r, A1, Xk, Xv, B),
	    {T,Min,Dec1};
       true -> {{r,A1,Xk,Xv,B},Min,false}
    end.

blackify({r,A,K,V,B}) -> {{b,A,K,V,B},false};
blackify(Node) -> {Node,true}.

unballeft(r, {b,A,Xk,Xv,B}, Yk, Yv, C) ->
    {lbalance(b, {r,A,Xk,Xv,B}, Yk, Yv, C),false};
unballeft(b, {b,A,Xk,Xv,B}, Yk, Yv, C) ->
    {lbalance(b, {r,A,Xk,Xv,B},Yk, Yv, C),true};
unballeft(b, {r,A,Xk,Xv,{b,B,Yk,Yv,C}}, Zk, Zv, D) ->
    {{b,A,Xk,Xv,lbalance(b, {r,B,Yk,Yv,C}, Zk, Zv, D)},false}.

unbalright(r, A, Xk, Xv, {b,B,Yk,Yv,C}) ->
    {rbalance(b, A, Xk, Xv, {r,B,Yk,Yv,C}),false};
unbalright(b, A, Xk, Xv, {b,B,Yk,Yv,C}) ->
    {rbalance(b, A, Xk, Xv, {r,B,Yk,Yv,C}),true};
unbalright(b, A, Xk, Xv, {r,{b,B,Yk,Yv,C},Zk,Zv,D}) ->
    {{b,rbalance(b, A, Xk, Xv, {r,B,Yk,Yv,C}), Zk, Zv, D},false}.

-spec fold(fun((any(), any(), any()) -> any()), any(), rbdict()) -> any().

%% fold(Fun, Acc, Dict) -> Acc.
%%  Fold Fun over Dict starting with value Acc.

fold(_, Acc, empty) -> Acc;
fold(F, Acc, {_,A,Xk,Xv,B}) ->
    fold(F, F(Xk, Xv, fold(F, Acc, B)), A).

-spec map(fun((any(), any()) -> any()), rbdict()) -> rbdict().

%% map(Fun, Dict) -> Dict.

map(_, empty) -> empty;
map(F, {RB,A,Xk,Xv,B}) ->
    {RB,map(F,A),Xk,F(Xk, Xv),map(F, B)}.

-spec filter(fun((any(), any()) -> bool()), rbdict()) -> rbdict().

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

-spec merge(fun((any(), any(), any()) -> any()), rbdict(), rbdict()) ->
    rbdict().

merge(F, D1, D2) ->
    fold(fun (K, V2, D) ->
		 update(K, fun(V1) -> F(K, V1, V2) end, V2, D)
	 end, D1, D2).				   

%% Extended interface

-spec foreach(fun((any(), any()) -> any()), rbdict()) -> ok.

%% foreach(Fun, Dict) -> ok.
%%  Apply Fun to each element in Dict.

foreach(_, empty) -> ok;
foreach(F, {_,A,Xk,Xv,B}) ->
    %% Do it left to right, even if this is not specified.
    foreach(F, A),
    F(Xk, Xv),
    foreach(F, B).

-spec all(fun((any(), any()) -> bool()), rbdict()) -> bool().

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

-spec any(fun((any(), any()) -> bool()), rbdict()) -> bool().

%% any(Pred, Dict) -> bool().

any(Pred, Dict) when is_function(Pred, 2) -> any_aux(Pred, Dict).

any_aux(_, empty) -> false;
any_aux(Pred, {_,A,Xk,Xv,B}) ->
    Pred(Xk, Xv) orelse any_aux(Pred, A) orelse any_aux(Pred, B).

-spec iter(fun((any(), any(), fun(() -> any())) -> any()),
           any(),
           rbdict()) -> any().

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
            rbdict()) -> any().

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

check(T) -> check(T, r).

check(empty, _) -> 1;
check({r,A,Xk,Xv,B}, b) ->		       	%Must have black parent
    case {check(A, r),check(B, r)} of
	{D,D}-> D;
	{Dl,Dr} -> exit({depth,{r,Dl,Xk,Xv,Dr}})
    end;
check({r,_,Xk,Xv,_}, r) ->		       	%Must have black parent
    exit({parent,{r,'-',Xk,Xv,'-'}});
check({b,A,Xk,Xv,B}, _) ->
    case {check(A, b),check(B,b)} of
	{D,D}-> D+1;				%Increase depth
	{Dl,Dr} -> exit({depth,{b,Dl,Xk,Xv,Dr}})
    end.

check_depth(T) -> check_depth(T, 1, orddict:new()).

check_depth(empty, D, Dd) ->
    orddict:update_counter(D, 1, Dd);
check_depth({_,A,_,_,B}, D, Dd0) ->
    Dd1 = orddict:update_counter(D, 1, Dd0),
    Dd2 = check_depth(A, D+1, Dd1),
    check_depth(B, D+1, Dd2).

t(Ks) ->
    lists:foldl(fun (K, D) -> store(K, K, D) end, new(), Ks).

%% Known error cases which have been fixed.

r1() ->
    {{b,{b,empty,37,37,empty},
       38,
       38,
       {b,{r,empty,39,39,empty},40,40,empty}},
     39,
     {b,{r,empty,37,37,empty},38,38,{b,empty,40,40,empty}}}.

r2() ->
    {{b,{r,{b,empty,43,43,empty},
	   46,
	   46,
	   {b,empty,48,48,empty}},
	50,
	50,
	{b,empty,53,53,empty}},
     53,
     {b,{b,empty,43,43,empty},
	46,
	46,
	{r,{b,empty,48,48,empty},50,50,empty}}}.
-endif.
