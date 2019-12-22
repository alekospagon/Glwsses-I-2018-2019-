
          
%reading stuff. ignore
code_char(A,B) :- char_code(B,A).

read_string1(Stream, Res) :-
    read_line_to_codes(Stream, Codes),
    Codes \= "\n",
    maplist(code_char, Codes, Char_list),
    string_to_list(Res, Char_list).


read_line(Stream, L) :-     %reads integers from a line
    read_line_to_codes(Stream, Line),
    atom_codes(Atom, Line),
    atomic_list_concat(Atoms, ' ', Atom),
    maplist(atom_number, Atoms, L).

read_line2(Stream, L) :-    %reads string from a line
    read_string1(Stream, L).

read_lines(Stream, 1, [L]) :-           %ending recusrion
    read_string1(Stream,  Original_L),
    string_to_list(Original_L, String_of_original_l),
    reverse(String_of_original_l, Rev),
    string_to_list(L, Rev).

read_lines(Stream, Counter, [X|L]) :-   %recursion
    read_string1(Stream, Original_X),
    string_to_list(Original_X, String_of_original_x),
    reverse(String_of_original_x, Rev),
    string_to_list(X, Rev),
    New_counter is Counter-1,
    read_lines(Stream, New_counter, L).

read_input(File, K, N, Q, Laxnoi, Tuxeroi) :-   %read whole file
    open(File, read, Stream),
    read_line(Stream, [K, N, Q]),
    read_lines(Stream, N, Laxnoi),
    read_lines(Stream, Q, Tuxeroi).

%here comes Trie_implementation.hack
%To Trie einai to idio trie pou ftiaksame se sml
%tropopoihmeno me aima kai idrwta apo empeirous
%hackers.

%+++++++++ INSERT +++++++++%


%to insertChild vazei psanxei se mia lista poio gramma 
%tairiazei (an tairiazei). an to vrei kalei thn 
%insert me ton kombo auto gia na sunexisei apo kei kai pera
%to insert proxwraei apo kombo se kombo

my_trie_insertChild([], [], _).

my_trie_insertChild([], [Letter|Rest], Res) :-
    my_trie_insertChild([], Rest, New_Res),
    Res = [1, Letter, New_Res].

my_trie_insertChild( [[So_far, Letter, B]|Lst] , [Letter|Rest] , Res) :-
    New_so_far is So_far + 1,
    my_trie_insert([New_so_far, Letter, B], Rest, My_Res),
    Res = [My_Res | Lst].

my_trie_insertChild( [[So_far, Letter_, B]|Lst], [Letter|Rest], Res) :-
    my_trie_insertChild( Lst, [Letter|Rest] , My_Res ),
      (
      My_Res = [_,_,[]] -> Res = [[So_far, Letter_, B] , My_Res ]
      ; Res = [[So_far, Letter_, B] | My_Res ]
       ).

my_trie_insertChild( [So_far, Letter, B] , [Letter|Rest], Res) :-
    New_so_far is So_far + 1,
    my_trie_insert([New_so_far, Letter, B] , Rest, Res).

my_trie_insertChild( [So_far, Letter_, B] , [Letter|Rest], Res ) :-
    my_trie_insertChild( [], [Letter|Rest] , My_Res ),
    Res = [[So_far, Letter_, B] , My_Res].

my_trie_insertChild( [So_far, Letter, B], [Letter], Res ) :-
    New_so_far is So_far + 1,
    my_trie_insert([New_so_far, Letter, B], [] , Res).

my_trie_insertChild( [So_far, Letter_, B], [Letter], Res) :-
    my_trie_insertChild( [], [Letter], My_Res ),
    Res = [[So_far, Letter_, B] , My_Res].

my_trie_insertChild( [[_, _, _]|_] , [] , []).



%Root Node only two elements
my_trie_insert( [So_far, LST] , [] , [New_so_far , LST]) :-
    New_so_far is So_far + 1.

%again Root
my_trie_insert( [So_far, LST] , Key, [New_so_far, Child]) :-
    New_so_far is So_far + 1,
    my_trie_insertChild(LST, Key, Child).

%Node with no Key left.
my_trie_insert( [So_far,  Letter, LST], [] , [So_far, Letter, LST]).

%Node with Key left
my_trie_insert( [So_far, Letter, LST] , Key , [So_far, Letter, Child]) :-
    my_trie_insertChild( LST, Key, Child ).

%i have char_code char-->ascii
%so i make ascii--> char
%code_char(A, B) :- char_code(B, A).

%The function that programmer calls and starts insertion
my_trie_final_insert( Trie, Key , Res) :-    
    %Key from string to char codes list
    string_to_list(Key, Exploded_Key),
    %char codes to real characters
    maplist(code_char, Exploded_Key, Real_char_key),
    my_trie_insert( Trie, Real_char_key, Res).

trie_init([Laxnos], [], Res) :-
    once(my_trie_final_insert([0,[]] , Laxnos, Res)).
trie_init([Laxnos], Trie_Here, Res) :-
    once(my_trie_final_insert(Trie_Here, Laxnos , Res)).
trie_init([Laxnos|Rest_Laxnoi], [], Res) :-
    once(my_trie_final_insert( [0, []] , Laxnos, My_Res1)),
    trie_init(Rest_Laxnoi, My_Res1, Res).
trie_init([Laxnos|Rest_Laxnoi], Trie_Here, Res) :-
    once(my_trie_final_insert(Trie_Here, Laxnos , My_Res1)),
    trie_init(Rest_Laxnoi, My_Res1, Res).

%implementation done

eval_List( [] , Out_of_Trie_Remaining_Key, Iters, Prev_cnt, Res_so_far, X, [X,B]) :-
    %UPDATE RES AND RETURN 
    %matched letters till out of Trie
    B is (Res_so_far + (Prev_cnt * (^(2, Iters) - 1))) mod 1000000007.
    
eval_List([[Cnt, LetA, Hanging_children] | LST] , [LetB|Rest], Iters, Prev_cnt, Res_so_far, X, [A,B]) :-


    (LetA = LetB) -> 
        (

            (Cnt = Prev_cnt) -> 
            (
                (New_X is max(X, Cnt)),
                eval([Cnt, LetA, Hanging_children], Rest, Iters , Prev_cnt, Res_so_far, New_X, [A,B])
            )
            ;
            (
                (New_X is max(X, Cnt)),
                New_res_so_far is Res_so_far + ((Prev_cnt-Cnt) * (^(2, Iters) - 1)),
                eval([Cnt, LetA, Hanging_children], Rest, Iters , Cnt, New_res_so_far, New_X, [A,B])
            )
        )
    ;
    %different letter so keep looking in More_nodes
    eval_List(LST, [LetB|Rest], Iters, Prev_cnt, Res_so_far, X, [A,B]).
    
eval_List([Cnt, Let, Hanging_children] , [Let|Rest], Iters, Prev_cnt, Res_so_far, X, [A,B]) :-
    (Cnt = Prev_cnt) -> 
    (
        (New_X is max(X, Cnt)),
        eval([Cnt, Let, Hanging_children], Rest, Iters , Prev_cnt, Res_so_far, New_X, [A,B])
    )
    ;
    (
        (New_X is max(X, Cnt)),
        New_res_so_far is Res_so_far + ((Prev_cnt-Cnt) * (^(2, Iters) - 1)),
        eval([Cnt, Let, Hanging_children], Rest, Iters , Cnt, New_res_so_far, New_X, [A,B])
    ).

eval_List( _, _, Iters, Prev_cnt, Res_so_far, X, [X,B]):-
    %CHECK AGAIN FOR THIS WEIRD PREDICATE
    B is (Res_so_far + (Prev_cnt * (^(2, Iters) - 1))) mod 1000000007.



eval( [Cnt, _, _] , [] , Iters, Prev_cnt, Res_so_far, X, [X,B]) :-
    B is (Res_so_far + (Cnt * (^(2, Iters+1) - 1))) mod 1000000007.

eval( [_, _, Hanging_children], Remaining_laxeio, Iters, Prev_cnt, Res_so_far, X, Res) :-
    New_iters is Iters + 1,
    eval_List(Hanging_children, Remaining_laxeio, New_iters, Prev_cnt, Res_so_far, X, Res).


final_eval([Node_info, First_Nodes], Tuxero, Res) :-
    string_to_list(Tuxero, Exploded_Tuxero),
    maplist(code_char, Exploded_Tuxero, Real_char_Tuxero),
    eval_List(First_Nodes, Real_char_Tuxero, 0, Node_info , 0 , 0, Res ).




%apotimish ola ta laxeia
loop_through_tuxera([Tuxero], My_Trie, [My_Res]) :-
    once(final_eval(My_Trie, Tuxero, My_Res)).

loop_through_tuxera([Tuxero|Rest_Tuxera], My_Trie, [Tuxero_Res | Rest_Res]) :-
    once(final_eval(My_Trie, Tuxero , Tuxero_Res)),
    loop_through_tuxera(Rest_Tuxera, My_Trie, Rest_Res).


lottery(File, Res) :-
    once(read_input(File, _, _, _, Laxnoi, Tuxeroi)),
    once(trie_init(Laxnoi, [], My_Trie)),
    once(loop_through_tuxera(Tuxeroi, My_Trie, Res)).
