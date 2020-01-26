:- use_module(library(thread)).

%+++++++++ 1) READ +++++++++%

%reading stuff. ignore
code_char(A,B) :- char_code(B,A).

read_line(Stream, L) :-     %reads integers from a line
    read_line_to_codes(Stream, Line),
    atom_codes(Atom, Line),
    atomic_list_concat(Atoms, ' ', Atom),
    maplist(atom_number, Atoms, L).


read_lines(_, 0, []) :- !.
read_lines(Stream, Counter, [X|L]) :-   %recursion
    read_line_to_codes(Stream, Codes),		%read
    maplist(code_char, Codes, Char_list),	%take codes
    reverse(Char_list, X),					%reverse

    New_counter is Counter-1,
    read_lines(Stream, New_counter, L).


read_input(File, Laxnoi, [T1, T2, T3, T4]) :-   %read whole file
    open(File, read, Stream),
    read_line(Stream, [_, N, Q]),
    read_lines(Stream, N, Laxnoi),
	
	%split Q in half but if Q is odd the extra bit
	%found in bit[0] (= Q /\ 1) are gonna go to Half2
	Half1 is Q >> 1,
	Half2 is (Q >> 1) + (Q /\ 1),

	%split into 4 by spliting the same way Halfs to Qrts

	Qrt1 is Half1 >> 1,
	Qrt2 is (Half1 >> 1) + (Half1 /\ 1),

	Qrt3 is Half2 >> 1,
	Qrt4 is (Half2 >> 1) + (Half2 /\ 1),

    read_lines(Stream, Qrt1, T1),
    read_lines(Stream, Qrt2, T2),
    read_lines(Stream, Qrt3, T3),
    read_lines(Stream, Qrt4, T4).



%+++++++++ 2) INSERT +++++++++%

%put nothing on node tree -> gain nothing
insertChild( [], [], []  ) :- !.

%put letters in no tree -> make a tree recursively
insertChild( [], [Letter|Rest], [ n(1, Letter, Rec_Res)] ) :- !,
    insertChild( [], Rest, Rec_Res ).

%put letters in existing tree -> if matching update counter
insertChild( [n(So_far, Letter_, B)|LST] , [Letter|Rest], Res) :- !,
    Letter = Letter_ -> 
    (
        New_so_far is So_far + 1,
        insert( n(New_so_far, Letter_, B) , Rest, Recursion_Res ),

        Res = [Recursion_Res | LST]
    )
    ;
    (   %insert into another node and update list of Children = 1st argument of predicate
        insertChild(LST, [Letter|Rest], Recursion_Res),
        Res = [ n(So_far, Letter_, B) | Recursion_Res ]
    ). 



insert( r(So_far, LST), KEY , r(New_so_far, Rec_Res)) :- !,
    New_so_far is So_far + 1,
    insertChild(LST, KEY, Rec_Res).

insert( _, [] , _ ) :- !.

insert( n(So_far, Letter, LST), KEY, n(So_far, Letter, Rec_Res) ) :- 
    insertChild(LST, KEY, Rec_Res).


%+++++++++ 3) TRIE INIT +++++++++%

%loop and insert for every one

trie_init([Laxnos], [], Res) :- !,
    insert( r(0,[]) , Laxnos, Res).

trie_init([Laxnos], Trie_Here, Res) :- 
    insert(Trie_Here, Laxnos , Res).

trie_init([Laxnos|Rest_Laxnoi], [], Res) :- !,
    insert( r(0,[]) , Laxnos, My_Res1),
    trie_init(Rest_Laxnoi, My_Res1, Res).

trie_init([Laxnos|Rest_Laxnoi], Trie_Here, Res) :- 
    insert(Trie_Here, Laxnos , My_Res1),
    trie_init(Rest_Laxnoi, My_Res1, Res).

%+++++++++ 4) EVAL +++++++++%

%descent down the Trie to evaluate. Every time counter decreases we update result

%out of Trie. stop.
eval( [] , _, Iters, Prev_cnt, Res_so_far, X, [X,B]) :-
	!,
    B is (Res_so_far + (Prev_cnt * (^(2, Iters) - 1))) mod 1000000007.

eval( [n(Cnt, LetA, Chld) | LST], [LetB|Rest] , Iters, Prev_cnt, Acc, X, [A,B] ) :-
	% follow this branch
    (LetA = LetB) -> 
        (
			% all words continue down this branch
            (Cnt = Prev_cnt) -> 
            (
				% needed. I start with 0 on root. The first branch I found
				% thats how many words I can match (even for earnings of 1)
                (New_X is max(X, Cnt)),
				New_iters is Iters + 1,
                eval(Chld, Rest, New_iters , Prev_cnt, Acc, New_X, [A,B])
            )
            ;
            (
                (New_X is max(X, Cnt)),
                New_acc is Acc + ((Prev_cnt-Cnt) * (^(2, Iters) - 1)),
				New_iters is Iters + 1,
                eval(Chld, Rest, New_iters , Cnt, New_acc, New_X, [A,B])
            )
        )
    ;
    %different letter so keep looking in More_nodes
    eval(LST, [LetB|Rest], Iters, Prev_cnt, Acc, X, [A,B]).



final_eval(r(Root_cnt, Nodes), Tuxero, Res) :-
	eval(Nodes, Tuxero, 0, Root_cnt , 0 , 0, Res ).



%+++++++++ 5) LOOP TO EVALUATE +++++++++%

loop_through_tuxera([], _, []) :- !.

loop_through_tuxera([Tuxero|Tuxera], My_Trie, [Res | Rest]) :-
    final_eval(My_Trie, Tuxero , Res),
    loop_through_tuxera(Tuxera, My_Trie, Rest).


%+++++++++ 6) SOLVE INPUT +++++++++%

lottery(File, Res) :-
    read_input(File, Laxnoi, [T1,T2, T3, T4]),
    trie_init(Laxnoi, [], My_Trie),
	
	Goals_solve = [
			loop_through_tuxera(T1, My_Trie, R1),
			loop_through_tuxera(T2, My_Trie, R2),
			loop_through_tuxera(T3, My_Trie, R3),
			loop_through_tuxera(T4, My_Trie, R4)
			],

	concurrent(4, Goals_solve, []),

	Goals_merge = [
			append(R1, R2, R12),
			append(R3, R4, R34)
			],

	concurrent(4, Goals_merge, []),

	append(R12, R34, Res).
