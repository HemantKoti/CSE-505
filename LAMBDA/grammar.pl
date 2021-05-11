
load(File) :-
        retractall(fun(_,_)),
        open(File,read,Stream),
        lex(Stream,Tokens),
        program(Tokens, []),
        !.
        


% ___________GRAMMAR RULES _______________


program --> ['#'].

program --> [let], [id(Name)], ['='], term(T),
            {asserta(fun(Name,T))},
            program.


term(V) --> var(V).

term(N) --> int(N).

term(l(V,T)) --> ['L'], var(V), ['.'], term(T).

term(a(T1,T2)) --> ['('], term(T1), term(T2), [')'].

int(X)  --> [num(X)].

var(X) --> [id(X)].


% ___________ LEXICAL ANALYZER  _______________________


lex(Stream,Tokens)  :-  get_chars(Stream,L), tokenize(L,Tokens),!.

get_chars(Str,L) :-  get_code(Str,C), get_chars(Str,C,L).

get_chars(_,35, [35]) :- !.				% 35 = #
get_chars(Str,C,  [C|L1]) :- get_chars(Str,L1).

tokenize([], []).
tokenize([C|L], L3)	:- white(C), skip_whites(L,L2), tokenize(L2,L3).
tokenize([37|L], L3)    :- skip_comment(L,L2), tokenize(L2,L3).
tokenize([76|L], ['L'|L2]) :- tokenize(L,L2).
tokenize([C|L], [X|L3]) :- alpha(C), identifier(X,[C|L],L2), tokenize(L2,L3).
tokenize([C|L], [X|L3]) :- d09(C), digits(X,[C|L],L2), tokenize(L2,L3).
tokenize(L, [X|L3])     :- special(X,L,L2), tokenize(L2,L3).

skip_whites([], []).
skip_whites([C|L], L2) :- (white(C) -> skip_whites(L,L2); L2 = [C|L]).

skip_comment([], []).
skip_comment([10|L], L) :- !.   % 10 is newline
skip_comment([_|L], L2) :- skip_comment(L,L2).


keyword(let).
keyword(derive).
keyword(norm).
keyword(step).
keyword(subst).
keyword(alpha).
keyword(reduce).
keyword(beta).
keyword(normal).
keyword(end).

identifier(X) --> ident(L),
                  {name(N,L),
                   (keyword(N) -> X = N ; X = id(N))
                  }.

ident([X|L]) --> letter(X), legits(L).
ident([X])   --> letter(X).

legits([X|W]) --> legit(X), legits(W).
legits([X])   --> legit(X).

legit(X) --> letter(X) ; digit(X).

letter(X) --> [X],  {alpha(X)}.

alpha(X) :-  X > 64,  X < 91.
alpha(X) :-  X > 96,  X < 123.

digits(num(N)) --> digs(L), {name(N,L)}.

digs([X|L]) --> digit(X), digs(L).
digs([X]) --> digit(X).

digit(X) -->  [X],  {d09(X)}.
d09(X) :- X > 47,  X < 58.

white(9).  % tab
white(10). % newline
white(32). % blank
white(13). % carriage return -added by Jinesh

special(=,[61|L],L).
special('(',[40|L],L).
special(')',[41|L],L).
special('[',[91|L],L).
special(']',[93|L],L).
special(.,[46|L],L).
special('<-', [60,45|L],L).
special('#', [35|L], L).
