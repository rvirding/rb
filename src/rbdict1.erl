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

-module(rbdict1).

-export([new/0,is_key/2,to_list/1,from_list/1,size/1]).
-export([fetch/2,find/2,fetch_keys/1,erase/2]).
-export([store/3,append/3,append_list/3]).
-export([update_val/3,update/3,update/4,update_counter/3]).
-export([fold/3,map/2,filter/2,merge/3]).

%% Deprecated interface.
-export([dict_to_list/1,list_to_dict/1]).
-deprecated([{dict_to_list,1},{list_to_dict,1}]).

-ifdef(DEBUG).
-export([check/1,erase_check/2,t/1,r1/0,r2/0]).
-endif.

%% -compile([export_all]).

%% {r,Left,K,V,Right}
%% {b,Left,K,V,Right}
%% empty

-define(IS_RED(N), (is_tuple(N) andalso element(1, N) == r)).
%% -define(IS_BLACK(N), not (is_tuple(N) andalso element(1, N) == r)).
-define(IS_BLACK(N),
	((is_tuple(N) andalso (element(1, N) == b)) orelse (N == empty))).

-define(DBLACK(N), [b|N]).

%% new() -> Dict.

new() -> empty.

%% is_key(Key, Dict) -> true | false.

is_key(_, empty) -> false;
is_key(K, {_,Left,K1,_,_}) when K < K1 ->
    is_key(K, Left);
is_key(K, {_,_,K1,_,Right}) when K > K1 ->
    is_key(K, Right);
is_key(_, {_,_,_,_,_}) -> true.

%% to_list(Dict) -> [{Key,Value}].

to_list(T) -> to_list(T, []).

to_list(empty, List) -> List;
to_list({_,A,Xk,Xv,B}, List) ->
    to_list(A, [{Xk,Xv}|to_list(B, List)]).

%% from_list([{Key,Value}]) -> Dict.

from_list(L) ->
    fold(fun ({K,V}, D) -> store(K, V, D) end, new(), L).

%% size(Dict) -> int().

size(T) -> size1(T).

size1(empty) -> 0;
size1({_,L,_,_,R}) ->
    size1(L) + size1(R) + 1.

%% fetch(Key, Dict) -> Value.

fetch(K, {_,Left,K1,_,_}) when K < K1 ->
    fetch(K, Left);
fetch(K, {_,_,K1,_,Right}) when K > K1 ->
    fetch(K, Right);
fetch(_, {_,_,_,Val,_}) -> Val.

%% find(Key, Dict) -> {ok,Value} | error.

find(_, empty) -> error;
find(K, {_,Left,K1,_,_}) when K < K1 ->
    find(K, Left);
find(K, {_,_,K1,_,Right}) when K > K1 ->
    fetch(K, Right);
find(_, {_,_,_,Val,_}) -> {ok,Val}.

%% fetch_keys(Dict) -> [Key].

fetch_keys(T) -> fetch_keys(T, []).

fetch_keys(empty, Tail) -> Tail;
fetch_keys({_,L,K,_,R}, Tail) ->
    fetch_keys(L, [K|fetch_keys(R, Tail)]).

%% store(Key, Val, Dict) -> Dict.

store(K, V, T) ->
    {_,L,K1,V1,R} = store1(K, V, T),
    {b,L,K1,V1,R}.				%setelement(1, b, T1).

store1(K, V, empty) -> {r,empty,K,V,empty};
store1(K, V, {C,Left,K1,V1,Right}) when K < K1 ->
    lbalance(C, store1(K, V, Left), K1, V1, Right);
store1(K, V, {C,Left,K1,V1,Right}) when K > K1 ->
    rbalance(C, Left, K1, V1, store1(K, V, Right));
store1(K, V, {C,L,_,_,R}) ->
    {C,L,K,V,R}.

%% append(Key, Val, Dict) -> Dict.

append(K, V, T) ->
    {_,L,K1,V1,R} = append1(K, V, T),
    {b,L,K1,V1,R}.				%setelement(1, b, T1).

append1(K, V, empty) -> {r,empty,K,[V],empty};
append1(K, V, {C,Left,K1,V1,Right}) when K < K1 ->
    lbalance(C, append1(K, V, Left), K1, V1, Right);
append1(K, V, {C,Left,K1,V1,Right}) when K > K1 ->
    rbalance(C, Left, K1, V1, append1(K, V, Right));
append1(K, V, {C,L,_,V1,R}) -> {C,L,K,V1 ++ [V],R}.

%% append(Key, [Val], Dict) -> Dict.

append_list(K, V, T) ->
    {_,L,K1,V1,R} = append_list1(K, V, T),
    {b,L,K1,V1,R}.				%setelement(1, b, T1).

append_list1(K, V, empty) -> {r,empty,K,V,empty};
append_list1(K, V, {C,Left,K1,V1,Right}) when K < K1 ->
    lbalance(C, append_list1(K, V, Left), K1, V1, Right);
append_list1(K, V, {C,Left,K1,V1,Right}) when K > K1 ->
    rbalance(C, Left, K1, V1, append_list1(K, V, Right));
append_list1(K, V, {C,L,_,V1,R}) -> {C,L,K,V1 ++ V,R}.

%% update_val(Key, Val, Dict) -> Dict.

update_val(K, V, {RB,A,Xk,Xv,B}) when K < Xk ->
    {RB,update_val(K, V, A),Xk,Xv,B};
update_val(K, V, {RB,A,Xk,Xv,B}) when K > Xk ->
    {RB,A,Xk,Xv,update_val(K, V, B)};
update_val(_, V, {RB,A,Xk,_,B}) ->
    {RB,A,Xk,V,B}.

%% update(Key, Fun, Dict) -> Dict.

update(K, F, {RB,A,Xk,Xv,B}) when K < Xk ->
    {RB,update(K, F, A),Xk,Xv,B};
update(K, F, {RB,A,Xk,Xv,B}) when K > Xk ->
    {RB,A,Xk,Xv,update(K, F, B)};
update(_, F, {RB,A,Xk,Xv,B}) ->
    {RB,A,Xk,F(Xv),B}.

%% update(Key, Fun, Init, Dict) -> Dict.

update(K, F, I, T) ->
    {_,L,K1,V1,R} = update1(K, F, I, T),
    {b,L,K1,V1,R}.				%setelement(1, b, T1).

update1(K, _, I, empty) -> {r,empty,K,I,empty};
update1(K, F, I, {RB,A,Xk,Xv,B}) when K < Xk ->
    lbalance(RB, update1(K, F, I, A), Xk, Xv, B);
update1(K, F, I, {RB,A,Xk,Xv,B}) when K > Xk ->
    rbalance(RB, A, Xk, Xv, update1(K, F, I, B));
update1(_, F, _, {RB,A,Xk,Xv,B}) ->
    {RB,A,Xk,F(Xv),B}.

%% update_counter(Key, Incr, Dict) -> Dict.

update_counter(K, I, T) ->
    {_,L,K1,V1,R} = update_counter1(K, I, T),
    {b,L,K1,V1,R}.				%setelement(1, b, T1).

update_counter1(K, I, empty) -> {r,empty,K,I,empty};
update_counter1(K, I, {RB,A,Xk,Xv,B}) when K < Xk ->
    lbalance(RB, update_counter1(K, I, A), Xk, Xv, B);
update_counter1(K, I, {RB,A,Xk,Xv,B}) when K > Xk ->
    rbalance(RB, A, Xk, Xv, update_counter1(K, I, B));
update_counter1(_, I, {RB,A,Xk,Xv,B}) ->
    {RB,A,Xk,Xv+I,B}.

%% lbalance(Colour, Left, Key, Val, Right).
%% rbalance(Colour, Left, Key, Val, Right).
%% Balance a tree afer (possibly) adding a node to the left/right.

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

add_token({r,L,K,V,R}) -> {b,L,K,V,R};
add_token(Node) -> ?DBLACK(Node).

%% erase(Key, Dict) -> Dict.

erase(K, T) ->
    case erase1(K, T) of
	{r,L1,K1,V1,R1} -> {b,L1,K1,V1,R1};	%setelement(1, b, T1).
	?DBLACK(X) -> X;
	Node -> Node
    end.

erase1(_, empty) -> empty;			%Not found
erase1(K, {r,empty,Xk,_,empty}=A) ->
    if  K < Xk -> A;				%Not found
	K > Xk -> A;				%Not found
	true -> empty				%Won't change balance
    end;
erase1(K, {b,empty,Xk,_,empty}=A) ->
    if  K < Xk -> A;				%Not found
	K > Xk -> A;				%Not found
	true -> add_token(empty)		%This is it
    end;
erase1(K, {r,A,Xk,Xv,B}=X) ->
    if  K < Xk ->
	    balleft(r, erase1(K, A), Xk, Xv, B);
	K > Xk ->
	    balright(r, A, Xk, Xv, erase1(K, B));
	true ->					%This is it
	    raise_pred_succ(X)
    end;
erase1(K, {b,A,Xk,Xv,B}=X) ->
    if  K < Xk ->
	    balleft(b, erase1(K, A), Xk, Xv, B);
	K > Xk ->
	    balright(b, A, Xk, Xv, erase1(K, B));
	true ->					%This is it
	    raise_pred_succ(X)
    end.

%% raise_pred_succ(Node) -> Node.

%% Remove and raise the successor node if the left branch is empty
%% else raise the predecessor node. Rebuild tree with removed node as
%% head making sure the resulting tree balanced. We KNOW that both
%% Left and Right cannot be empty.

raise_pred_succ({C,empty,_,_,B}) ->
	    {B1,{Xk1,Xv1}} = raise_succ(B),
	    balright(C, empty, Xk1, Xv1, B1);
raise_pred_succ({C,A,_,_,B}) ->
	    {A1,{Xk1,Xv1}} = raise_pred(A),
	    balleft(C, A1, Xk1, Xv1, B).

%% raise_pred(Node) -> {PredTree,{NodeKey,NodeVal}}.
%% raise_succ(Node) -> {SuccTree,{NodeKey,NodeVal}}.
%%  Remove and raise the predecessor/successor node rebalancing the
%%  tree when necessary.

raise_pred({b,A,Xk,Xv,empty}) -> {add_token(A),{Xk,Xv}};
raise_pred({r,A,Xk,Xv,empty}) -> {A,{Xk,Xv}};	%Won't change balance
raise_pred({b,A,Xk,Xv,B}) ->
    {B1,Pred} = raise_pred(B),
    {balright(b, A, Xk, Xv, B1),Pred};
raise_pred({r,A,Xk,Xv,B}) ->
    {B1,Pred} = raise_pred(B),
    {balright(r, A, Xk, Xv, B1),Pred}.
    
raise_succ({b,empty,Xk,Xv,A}) -> {add_token(A),{Xk,Xv}};
raise_succ({r,empty,Xk,Xv,A}) -> {A,{Xk,Xv}};	%Won't change balance
raise_succ({b,A,Xk,Xv,B}) ->
    {A1,Succ} = raise_succ(A),
    {balleft(b, A1, Xk, Xv, B),Succ};
raise_succ({r,A,Xk,Xv,B}) ->
    {A1,Succ} = raise_succ(A),
    {balleft(r, A1, Xk, Xv, B),Succ}.

%% balleft(Colour, Left, Key, Val, Right)
%% balright(Colour, Left, Key, Val, Right)
%% Rebalance a tree knowing that the left/right tree may have been
%% made smaller.

balleft(RB, ?DBLACK(A), Xk, Xv, {b,{r,B,Yk,Yv,C},Zk,Zv,D})
  when ?IS_BLACK(A) ->
    %%io:fwrite("LA(~w)\n", [Xk]),
    {RB,{b,A,Xk,Xv,B},Yk,Yv,{b,C,Zk,Zv,D}};
balleft(RB, ?DBLACK(A), Xk, Xv, {b,B,Yk,Yv,{r,C,Zk,Zv,D}})
  when ?IS_BLACK(A) ->
    {RB,{b,A,Xk,Xv,B},Yk,Yv,{b,C,Zk,Zv,D}};
balleft(RB, ?DBLACK(A), Xk, Xv, {b,B,Yk,Yv,C})
  when ?IS_BLACK(A) and ?IS_BLACK(B) and ?IS_BLACK(C) ->
    add_token({RB,A,Xk,Xv,{r,B,Yk,Yv,C}});
balleft(b, ?DBLACK(A), Xk, Xv, {r,B,Yk,Yv,C})
  when ?IS_BLACK(A) and ?IS_BLACK(B) and ?IS_BLACK(C) ->
    balleft(b, balleft(r, ?DBLACK(A), Xk, Xv, B), Yk, Yv, C);
%% No rule matches, just pass double black up the tree.
balleft(RB, ?DBLACK(A), Xk, Xv, B) -> add_token({RB,A,Xk,Xv,B});
balleft(RB, A, Xk, Xv, B) -> {RB,A,Xk,Xv,B}.

balright(RB, {b,{r,A,Xk,Xv,B},Yk,Yv,C}, Zk, Zv, ?DBLACK(D))
  when ?IS_BLACK(D) ->
    %%io:fwrite("RA(~w)\n", [Zk]),
    {RB,{b,A,Xk,Xv,B},Yk,Yv,{b,C,Zk,Zv,D}};
balright(RB, {b,A,Xk,Xv,{r,B,Yk,Yv,C}}, Zk, Zv, ?DBLACK(D))
  when ?IS_BLACK(D) ->
    {RB,{b,A,Xk,Xv,B},Yk,Yv,{b,C,Zk,Zv,D}};
balright(RB, {b,A,Xk,Xv,B}, Yk, Yv, ?DBLACK(C))
  when ?IS_BLACK(A) and ?IS_BLACK(B) and ?IS_BLACK(C) ->
    add_token({RB,{r,A,Xk,Xv,B},Yk,Yv,C});
balright(b, {r,A,Xk,Xv,B}, Yk, Yv, ?DBLACK(C))
  when ?IS_BLACK(A) and ?IS_BLACK(B) and ?IS_BLACK(C) ->
    balright(b, A, Xk, Xv, balright(r, B, Yk, Yv, ?DBLACK(C)));
%% No rule matches, just pass double black up the tree.
balright(RB, A, Xk, Xv, ?DBLACK(B)) -> add_token({RB,A,Xk,Xv,B});
balright(RB, A, Xk, Xv, B) -> {RB,A,Xk,Xv,B}.

%% fold(Fun, Acc, Dict) -> Acc.

fold(_, Acc, empty) -> Acc;
fold(F, Acc, {_,A,Xk,Xv,B}) ->
    fold(F, F(Xk, Xv, fold(F, Acc, B)), A).

%% map(Fun, Dict) -> Dict.

map(_, empty) -> empty;
map(F, {RB,A,Xk,Xv,B}) ->
    {RB,map(F,A),Xk,F(Xk, Xv),map(F, B)}.

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

merge(F, D1, D2) ->
    fold(fun (K, V2, D) ->
		 update(K, fun(V1) -> F(K, V1, V2) end, V2, D)
	 end, D1, D2).				   

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
