

% ======================================================================================= %
% ======================================================================================= %

% Implementation of AVL trees in prolog
% Stolen from https://github.com/salva/salva-prolog-modules/blob/master/avl.pl
% and added some adjustments to improve performance

%%  avl_empty(-Tree:avl) is det.
%
%  Creates an empty AVL tree.

avl_empty(t).


%% avl_put(+Tree:avl, +Key, -Out:avl) is semidet.
% Equivalent to avl_put(Tree, Key, [], Out).



%% avl_put(+Tree:avl, +Key:ground_term, +Value, -Out:avl) is semidet.
%
% Inserts the pair Key-Value into the tree.
%
% Fails when the tree already contains an element with the given key.

avl_put(t, K, V, avl(K, V, t, t, 1)).

avl_put(avl(NK, NV, L, R, D), K, V, T) :-
compare(O, K, NK),
(
	O=(<)->
	(
		avl_put(L, K, V, L1),
		(   L1 = avl(_, _, _, _, D)
			->  avl_balance_left(NK, NV, L1, R, T)
			;   T = avl(NK, NV, L1, R, D) 
		)
	)
	;
	(
		avl_put(R, K, V, R1),
		(   R1 = avl(_, _, _, _, D)
			->  avl_balance_right(NK, NV, L, R1, T)
			;   T = avl(NK, NV, L, R1, D) 
		)
	)
).




%% avl_replace(+Tree:avl, +Key, -Out:avl) is det.
% Equivalent to avl_replace(Tree, Key, [], Out).


%% avl_replace(+Tree:avl, +Key, +Value, -Out:avl) is det.
% Inserts the pair Key-Value into the tree.
%
% Replaces any previous entry with the given key.


avl_replace(t, K, V, avl(K, V, t, t, 1)).

avl_replace(avl(NK, NV, L, R, D), K, V, T) :-
compare(O, K, NK),
(
	O=(<)->
	(
		avl_replace(L, K, V, L1),
		(   L1 = avl(_, _, _, _, D)
			->  avl_balance_left(NK, NV, L1, R, T)
			;   T = avl(NK, NV, L1, R, D) 
		)
	)
	;
	O=(>)->
	(
		avl_replace(R, K, V, R1),
		(   R1 = avl(_, _, _, _, D)
			->  avl_balance_right(NK, NV, L, R1, T)
			;   T = avl(NK, NV, L, R1, D) 
		)
	)
	;T=avl(NK, V, L, R, D)
).






%% avl_depth(+Tree:avl, -Depth:int) is det.
% Returns the tree depth.

avl_depth(t, 0).
avl_depth(avl(_,_,_,_,D), D).


avl_cmp_depth(t, D, D).
avl_cmp_depth(avl(_,_,_,_,AD), BD, D) :-
D is BD - AD.

avl_balance_left(NK, NV, avl(LK, LV, LL, LR, LD), R, T) :-
(   avl_cmp_depth(R, LD, 2)
	->  % avl_dump(avl(NK, NV, avl(LK, LV, LL, LR, LD),R, _), 'lb: '),nl,
	(	LR = avl(LRK, LRV, LRL, LRR, LRD),
		avl_cmp_depth(LL, LRD, 1)
		->	T = avl(LRK, LRV, avl(LK, LV, LL, LRL, LRD), avl(NK, NV, LRR, R, LRD), LD)
		;	ND1 is LD-1,
		T = avl(LK, LV, LL, avl(NK, NV, LR, R, ND1), LD) )
	;   D1 is LD + 1,
	T = avl(NK, NV, avl(LK, LV, LL, LR, LD), R, D1) ).

avl_balance_right(NK, NV, L, avl(RK, RV, RL, RR, RD), T) :-
(   avl_cmp_depth(L, RD, 2)
	->  % avl_dump(avl(NK, NV, L, avl(RK, RV, RL, RR, RD), _), 'rb: '),nl,
	(	RL = avl(RLK, RLV, RLL, RLR, RLD),
		avl_cmp_depth(RR, RLD, 1)
		->	T = avl(RLK, RLV, avl(NK, NV, L, RLL, RLD), avl(RK, RV, RLR, RR, RLD), RD)
		;	ND1 is RD-1,
		T = avl(RK, RV, avl(NK, NV, L, RL, ND1), RR, RD) )
	;   D1 is RD + 1,
	T = avl(NK, NV, L, avl(RK, RV, RL, RR, RD), D1) ).


%% avl_get(+Tree:avl, +Key, -Value) is semidet.
% Retrieves the value associated to the given key.

avl_get(avl(NK, NV, L, R, _), K, V) :-
compare(O,NK,K),
(O=(>)->avl_get(L,K,V)
	;O=(<)->avl_get(R,K,V)
	;V=NV).


% AVL trees end here
% ===================================================================================== %
% ===================================================================================== %

% The following code reads the input from the given file
% Stolen from http://courses.softlab.ntua.gr/pl1/2019a/Exercises/read_colors_SWI.pl

read_line(Stream, L) :-
read_line_to_codes(Stream, Line),
atom_codes(Atom, Line),
atomic_list_concat(Atoms, ' ', Atom),
maplist(atom_number, Atoms, L).

read_input(File, N, K, C) :-
open(File, read, Stream),
read_line(Stream, [N, K]),
read_line(Stream, C).

% Input code ends here
% ===================================================================================== %
% ===================================================================================== %


begin_move_last(_,[],_,_,_,_,_,Res,Res).

begin_move_last(First,[Last|Last_rest],First_index,Last_index,Freq,K,Colors_used,Res_so_far,Res):-
New_last_index is Last_index+1,
(avl_get(Freq,Last,Last_color_freq)->
	(
		New_last_color_freq is Last_color_freq+1,
		avl_replace(Freq,Last,New_last_color_freq,New_freq),
		begin_move_last(First,Last_rest,First_index,New_last_index,New_freq,K,Colors_used,Res_so_far,Res)
	)
	;	
	(
		New_colors_used is Colors_used+1,
		avl_put(Freq,Last,1,New_freq),
		(New_colors_used=K->move_first(First,Last_rest,First_index,New_last_index,New_freq,Res_so_far,Res)
			;begin_move_last(First,Last_rest,First_index,New_last_index,New_freq,K,New_colors_used,Res_so_far,Res))
	)).


move_last(_,[],_,_,_,_,Res,Res).

move_last(First,[Last|Last_rest],First_index,Last_index,Freq,Missing,Res_so_far,Res):-
New_last_index is Last_index+1,
(\+Last=Missing->
	(
		avl_get(Freq,Last,Last_color_freq),
		New_last_color_freq is Last_color_freq+1,
		avl_replace(Freq,Last,New_last_color_freq,New_freq),
		move_last(First,Last_rest,First_index,New_last_index,New_freq,Missing,Res_so_far,Res)
	)
	;	
	(
		move_first(First,Last_rest,First_index,New_last_index,Freq,Res_so_far,Res)
	)).



move_first([First|First_rest],Last,First_index,Last_index,Freq,Res_so_far,Res):-	
New_first_index is First_index+1,
Dist is Last_index - First_index,
(Dist<Res_so_far->New_res_so_far =Dist
	;New_res_so_far=Res_so_far),
avl_get(Freq,First,First_color_freq),
(\+First_color_freq=1->
	(
		New_first_color_freq is First_color_freq-1,
		avl_replace(Freq,First,New_first_color_freq,New_freq),
		move_first(First_rest,Last,New_first_index,Last_index,New_freq,New_res_so_far,Res)	
	)
	;
	(

		move_last(First_rest,Last,New_first_index,Last_index,Freq,First,New_res_so_far,Res)
	)).



colors(File,Answer):-
once(solve(File,Answer)).

solve(File,Answer):-
read_input(File, N, K, List),
Fake_res is N+10,
avl_empty(Freq),
begin_move_last(List,List,1,1,Freq,K,0,Fake_res,Res),
once(\+Res=Fake_res-> Answer=Res
	;Answer=0).	        
