
          
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


    
%AVL trees end here
% ===================================================================================== %
% ===================================================================================== %

% The following code reads the input from the given file
% Stolen from http://courses.softlab.ntua.gr/pl1/2019a/Exercises/read_colors_SWI.pl
% and added some adjustments

read_line(Stream, L) :-
    read_line_to_codes(Stream, Line),
    atom_codes(Atom, Line),
    atomic_list_concat(Atoms, ' ', Atom),
    maplist(atom_number, Atoms, L).
    
    
read_input(File,Input):-
    open(File,read,Stream),
    read_line(Stream,[Q]),
    read_testcase(Stream,Q,[],Input).
    
read_testcase(_,0,So_far,Input):-
    reverse(So_far,Input).
    
read_testcase(Stream,Counter,So_far,Input):-
    read_line(Stream,[Lin,Rin,Lout,Rout]),
    New_counter is Counter-1,
    read_testcase(Stream,New_counter,[[Lin,Rin,Lout,Rout]|So_far],Input).
    
% Input code ends here
%=======================================================================================%	
%=======================================================================================%

ztalloc(File,Res):-
    once(read_input(File,Input)),
    once(next_testcase(Input,[],Res)).
    
next_testcase([],So_far,Res):-
    reverse(So_far,Res).

next_testcase([[Lin,Rin,Lout,Rout]|Rest_input],So_far,Final_res):-

    (
        (Lin@>=Lout,Rin@=<Rout) ->next_testcase(Rest_input,['EMPTY'|So_far],Final_res)
        ;Lin=Rin->
        (
            avl_empty(Seen),
            avl_put(Seen,Lin,-1,New_seen),
            known(New_seen,[Lin],[],Lout,Rout,Res),
            next_testcase(Rest_input,[Res|So_far],Final_res)
        )
        ;
        (
            avl_empty(Seen),
            avl_put(Seen,[Lin,Rin],-1,New_seen),
            unknown(New_seen,[[Lin,Rin]],[],Lout,Rout,Res),
            next_testcase(Rest_input,[Res|So_far],Final_res)
        )
    ).
        
        


known(_,[],[],_,_,'IMPOSSIBLE').

known(Seen,[],[X1|X],Lout,Rout,Res):-
    reverse([X1|X],Next_queue),
    known(Seen,Next_queue,[],Lout,Rout,Res).

known(Seen,[Current|Queue_rest],Next_queue,Lout,Rout,Res):-
    Triple is 3*Current+1,
    Half is Current//2,
    Key is 1+mod(Current,2),
    
    (avl_get(Seen,Half,_)->
    (
        (	
            (Triple<1000000,\+avl_get(Seen,Triple,_))->
            (
                avl_put(Seen,Triple,0,New_seen1),
                (
                    (Triple@>=Lout,Triple@=<Rout)->found_known(Seen,Current,t,Res)
                    ;
                    known(New_seen1,Queue_rest,[Triple|Next_queue],Lout,Rout,Res)
                )
            )
            ;
                known(Seen,Queue_rest,Next_queue,Lout,Rout,Res)
        )
    )
    ;
    (
        avl_put(Seen,Half,Key,New_seen1),
        (
            (Half@>=Lout,Half@=<Rout)->found_known(New_seen1,Current,h,Res)
            ;
            (
                (
                    (Triple<1000000,\+avl_get(New_seen1,Triple,_))->
                    (
                        (	
                            (Triple@>=Lout,Triple@=<Rout)->found_known(New_seen1,Current,t,Res)
                            ;
                            (
                                avl_put(New_seen1,Triple,0,New_seen2),
                                known(New_seen2,Queue_rest,[Triple,Half|Next_queue],Lout,Rout,Res)
                            )
                        )
                    )
                    ;
                        known(New_seen1,Queue_rest,[Half|Next_queue],Lout,Rout,Res)
                )
            )
        )
    )).
            
    
        
found_known(Seen,Current,So_far,Res):-
    avl_get(Seen,Current,Key),
    (Key = -1->Res=So_far
    ;Key=0->(Next is (Current-1)//3,(atom_concat(t,So_far,New_so_far),found_known(Seen,Next,New_so_far,Res)))
    ;(Next is Key-1+2*Current, atom_concat(h,So_far,New_so_far), found_known(Seen,Next,New_so_far,Res))).
    
    
% ====================================================================================================== %

unknown(_,[],[],_,_,'IMPOSSIBLE').

unknown(Seen,[],[X1|X],Lout,Rout,Res):-
    reverse([X1|X],Next_queue),
    unknown(Seen,Next_queue,[],Lout,Rout,Res).

unknown(Seen,[[Current1,Current2]|Queue_rest],Next_queue,Lout,Rout,Res):-
    Triple1 is 3*Current1+1,
    Triple2 is 3*Current2+1,
    Half1 is Current1//2,
    Half2 is Current2//2,
    Key is 1+2*mod(Current2,2)+mod(Current1,2),
    
    (avl_get(Seen,[Half1,Half2],_)->
    (
        (	
            (Triple2<1000000,\+avl_get(Seen,[Triple1,Triple2],_))->
            (
                avl_put(Seen,[Triple1,Triple2],0,New_seen1),
                (
                    (Triple1 @>= Lout,Triple2 @=< Rout)->found_unknown(Seen,[Current1,Current2],t,Res)
                    ;
                    unknown(New_seen1,Queue_rest,[[Triple1,Triple2]|Next_queue],Lout,Rout,Res)
                )
            )
            ;
                unknown(Seen,Queue_rest,Next_queue,Lout,Rout,Res)
        )
    )
    ;
    (
        avl_put(Seen,[Half1,Half2],Key,New_seen1),
        (
            (Half1 @>= Lout,Half2 @=< Rout)->found_unknown(New_seen1,[Current1,Current2],h,Res)
            ;
            (
                (
                    (Triple2<1000000,\+avl_get(New_seen1,[Triple1,Triple2],_))->
                    (
                        (	
                            (Triple1 @>= Lout,Triple2 @=< Rout)->found_unknown(New_seen1,[Current1,Current2],t,Res)
                            ;
                            (
                                avl_put(New_seen1,[Triple1,Triple2],0,New_seen2),
                                unknown(New_seen2,Queue_rest,[[Triple1,Triple2],[Half1,Half2]|Next_queue],Lout,Rout,Res)
                            )
                        )
                    )
                    ;
                        unknown(New_seen1,Queue_rest,[[Half1,Half2]|Next_queue],Lout,Rout,Res)
                )
            )
        )
    )).
            
    
        
found_unknown(Seen,[Current1,Current2],So_far,Res):-
    avl_get(Seen,[Current1,Current2],Key),
    (Key = -1->Res=So_far
    ;Key=0->
    (
        Next1 is (Current1-1)//3,
        Next2 is (Current2-1)//3,
        atom_concat(t,So_far,New_so_far),
        found_unknown(Seen,[Next1,Next2],New_so_far,Res)
    )
    ;
    (
        Kkey is Key-1,
        Next1 is mod(Kkey,2)+2*Current1,
        Next2 is Kkey//2+2*Current2,
        atom_concat(h,So_far,New_so_far),
        found_unknown(Seen,[Next1,Next2],New_so_far,Res)
    )).	
