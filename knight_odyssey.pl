/* Knight's Odyssey - A Dark RPG Adventure Game */

:- dynamic i_am_at/1, at/2, holding/1, sanity/1, hunger/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(holding(_)), retractall(sanity(_)), retractall(hunger(_)).

/* Initial game state */
i_am_at(dungeon_entrance).
sanity(100).
hunger(0).

/* Path connections */
path(dungeon_entrance, s, main_hall).
path(main_hall, n, dungeon_entrance).
path(main_hall, e, library).
path(library, w, main_hall).
path(main_hall, w, alchemy_lab).
path(alchemy_lab, e, main_hall).
path(main_hall, s, kitchen).
path(kitchen, n, main_hall).

/* Objects in locations */
at(torch, dungeon_entrance).
at(note, dungeon_entrance).
at(ancient_book, library).
at(acid_potion, alchemy_lab).
at(bread, kitchen).
at(wine, kitchen).

/* These rules describe how to pick up an object. */
take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('OK.'),
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.

/* These rules describe how to put down an object. */
drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        write('OK.'),
        !, nl.

drop(_) :-
        write('You aren''t holding it!'),
        nl.

/* These rules define the direction letters as calls to go/1. */
n :- go(n).
s :- go(s).
e :- go(e).
w :- go(w).

/* This rule tells how to move in a given direction. */
go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        !, look.

go(_) :-
        write('You can''t go that way.').

/* This rule tells how to look about you. */
look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl.

/* These rules set up a loop to mention all the objects in your vicinity. */
notice_objects_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).

/* This rule just writes out game instructions. */
instructions :-
        nl,
        write('Welcome to Knight''s Odyssey - A Dark RPG Adventure Game'), nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.             -- to start the game.'), nl,
        write('n.  s.  e.  w.     -- to go in that direction.'), nl,
        write('take(Object).      -- to pick up an object.'), nl,
        write('drop(Object).      -- to put down an object.'), nl,
        write('look.              -- to look around you again.'), nl,
        write('instructions.      -- to see this message again.'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        nl.

/* This rule prints out instructions and tells where you are. */
start :-
        instructions,
        look.

/* These rules describe the various rooms. */
describe(dungeon_entrance) :-
        write('You are at the entrance to the ancient catacombs. The stone steps lead down into darkness.'), nl,
        write('A cold draft whispers from below, carrying with it the scent of old stone and something else...'), nl.

describe(main_hall) :-
        write('You are in the main hall of the catacombs. The ceiling arches high above, lost in shadows.'), nl,
        write('Ancient stone pillars line the walls, their surfaces covered in strange markings.'), nl.

describe(library) :-
        write('You are in a vast library. Dust-covered bookshelves reach from floor to ceiling.'), nl,
        write('The air is thick with the smell of old parchment and leather bindings.'), nl.

describe(alchemy_lab) :-
        write('You are in an alchemy laboratory. Strange apparatuses bubble and hiss on wooden tables.'), nl,
        write('Colored liquids in glass vessels cast eerie shadows on the walls.'), nl.

describe(kitchen) :-
        write('You are in an ancient kitchen. A large hearth dominates one wall, its embers still glowing faintly.'), nl,
        write('The room is filled with the scent of old spices and something that might have been food long ago.'), nl.