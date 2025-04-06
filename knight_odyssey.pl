:- dynamic i_am_at/1, at/2, inventory/2, sanity/1, hunger/1, torch_lit/0, torch_remaining/1, decay_no_pickup/0, hallucinating/0, decaying_hands/0, decay_turns/1.

init_game :-
    retractall(at(_, _)),
    retractall(i_am_at(_)),
    retractall(inventory(_, _)),
    retractall(sanity(_)),
    retractall(hunger(_)),
    retractall(decay_no_pickup),
    retractall(hallucinating),
    retractall(decaying_hands),
    retractall(decay_turns(_)),
    assert(i_am_at(dungeon_entrance)),
    assert(at(torch, dungeon_entrance)),
    assert(at(dungeon_entrance_note, dungeon_entrance)),
    assert(at(statue, main_hall)),
    assert(at(pit, dark_room1)),
    assert(at(escape_attempt, pit_bottom)),
    assert(at(opium_powder, alchemy_lab)),
    assert(at(blue_vial, alchemy_lab)),
    assert(at(green_vial, alchemy_lab)),
    assert(at(purple_vial, alchemy_lab)),
    assert(at(red_vial, alchemy_lab)),
    assert(at(kitchen_knife, kitchen)),
    assert(at(rotten_meat, kitchen)),
    assert(at(fish, kitchen)),
    assert(at(bread, kitchen)),
    assert(at(wine_bottle, kitchen)),
    assert(at(priest, chapel)),
    assert(at(pyramid_artifact, chapel)),
    assert(sanity(100)),
    assert(hunger(0)).

path(dungeon_entrance, n, main_hall).
path(main_hall, s, dungeon_entrance).
path(main_hall, e, dark_room1).
path(dark_room1, beneath, pit_bottom).
path(dark_room1, w, main_hall).
path(main_hall, w, alchemy_lab).
path(alchemy_lab, e, main_hall).
path(main_hall, n, fork1).
path(fork1, s, main_hall).
path(fork1, w, kitchen).
path(kitchen, e, fork1).
path(fork1, e, chapel).
path(chapel, w, fork1).
path(chapel, s, library).
path(library, n, chapel).

examine(X) :-
    (inventory(X, Qty), Qty > 0) -> 
        examine_item(X),
        update_status
    ;
    i_am_at(Place),
    at(X, Place) ->
        examine_item(X),
        update_status
    ;
    write('You don''t see any '), write(X), write(' here to examine.'),
    nl.

examine_item(torch) :-
    write('A wooden torch soaked in pitch. It can provide light in the darkness, but will eventually burn out.'), nl,
    write('Traveling with a lit torch helps ward off the oppressive darkness, keeping your mind more peaceful.'), nl.

examine_item(dungeon_entrance_note) :-
    write('A bloodstained parchment with hurried, desperate writing. The message reads:'), nl,
    write('"I have located the Chalice in the deepest chamber of these accursed catacombs. The whispers... they'), nl,
    write('speak truths too terrible to bear. Abominations stalk these halls - things that were once men.'), nl,
    write('Three seals guard the inner sanctum. I have located one near the library archives, but the others...'), nl,
    write('The walls bleed. The shadows move when unwatched. If you read this, flee while your mind remains yours..."'), nl.

examine_item(statue) :-
    write('A grotesque effigy carved from obsidian-black stone. The deity it depicts defies comprehension -'), nl,
    write('part human, yet wrong in ways your mind struggles to process. Multiple limbs emerge at impossible'), nl,
    write('angles, and its face bears too many eyes, arranged in a pattern that makes your vision swim.'), nl,
    write('You\'ve never encountered anything resembling this entity in any religious text or traveler\'s tale.'), nl,
    write('The base of the statue contains three distinct recesses, shaped to hold three specific objects:'), nl,
    write('- a cube'), nl,
    write('- a pyramid'), nl,
    write('- a sphere'), nl,
    write('Looking closer, you notice scrape marks on the stone floor around the statue\'s base,'), nl,
    write('suggesting it has been moved recently. The pattern of the marks hints that the statue'), nl,
    write('might conceal something hidden beneath it.'), nl.

examine_item(pit) :-
    write('The pit descends into impenetrable darkness. You cannot see its bottom, even when'), nl,
    write('you carefully peer over the edge. The fetid smell makes you recoil involuntarily.'), nl,
    write('When you drop a small stone in, you hear it bouncing against the walls for several'), nl,
    write('seconds before a distant splash echoes up.'), nl.

examine_item(escape_attempt) :-
    write('You desperately claw at the walls, trying to climb out of this foul pit.'), nl,
    write('The stones are coated in centuries of filth and slime, offering no purchase.'), nl,
    write('Each attempt leaves you sliding back down into the fetid muck below.'), nl,
    write('After several exhausting tries, the terrible realization sinks in - there is no way out.'), nl,
    write('This place will be your tomb, just as it was for others before you.'), nl,
    decrease_sanity(100).

examine_item(opium_powder) :-
    write('A small jar containing a crude brownish powder derived from poppy seeds.'), nl,
    write('Such substances were used by medieval healers to dull pain, though none called it by this name.'), nl,
    write('The jar bears symbols suggesting it will calm the mind and ease suffering.'), nl.

examine_item(blue_vial) :-
    write('A vial of luminous blue liquid that seems to swirl of its own accord.'), nl,
    write('It smells faintly of sea salt and something unidentifiable.'), nl.

examine_item(green_vial) :-
    write('A viscous green substance that occasionally bubbles on its own.'), nl,
    write('It emits fumes that make your eyes water. The glass of the vial appears to be slowly'), nl,
    write('corroding from the inside.'), nl.

examine_item(purple_vial) :-
    write('A thick purple liquid with tiny specks of light floating within it.'), nl,
    write('The vial is pleasantly warm to the touch.'), nl.

examine_item(red_vial) :-
    write('A crimson fluid that looks disturbingly like blood, yet moves more slowly.'), nl,
    write('The vial pulses gently, almost like a heartbeat.'), nl.

examine_item(kitchen_knife) :-
    write('A large knife with a rusty blade and rotting wooden handle. Despite its condition,'), nl,
    write('it still holds a threatening edge. Flakes of what might be dried blood cling to the blade.'), nl.

examine_item(rotten_meat) :-
    write('A chunk of unidentifiable meat, green with mold and crawling with maggots.'), nl,
    write('The stench emanating from it makes your stomach turn. What kind of creature'), nl,
    write('this once was is impossible to determine.'), nl.

examine_item(fish) :-
    write('A dried fish, preserved with salt. While certainly not fresh,'), nl,
    write('it appears to be edible and free from obvious spoilage.'), nl,
    write('The flesh is firm and the smell, while strong, is not putrid.'), nl,
    write('It could provide sustenance if you''re desperate enough.'), nl.

examine_item(bread) :-
    write('A surprisingly intact loaf of dark bread. Though stale and hard as stone,'), nl,
    write('it appears free of mold and might still be edible. It would at least'), nl,
    write('quiet your growing hunger.'), nl.

examine_item(wine_bottle) :-
    write('An ancient bottle of wine, its label long rotted away. The dark liquid inside'), nl,
    write('sloshes thickly when you move it. The cork is partially decomposed but still'), nl,
    write('seals the contents. It might provide a temporary respite from the horrors around you.'), nl.

examine_item(priest) :-
    write('The priest''s skeletal form is draped in rotting ceremonial robes adorned with'), nl,
    write('strange symbols and dried bloodstains. His skin is pale and stretched too tightly'), nl,
    write('across his skull. His eyes never blink as they watch your every movement.'), nl,
    write('There is something unnatural about his movements - jerky, yet fluid, as if his body'), nl,
    write('is not entirely under his own control. His lips constantly move in silent prayer or curse.'), nl.

examine_item(dead_priest) :-
    write('The priest\'s corpse lies in a pool of black fluid that seems to move slightly of its own accord.'), nl,
    write('His face is frozen in that terrible smile, eyes still open and staring at nothing.'), nl,
    write('The wound in his chest pulses faintly, as if something inside is still alive.'), nl,
    write('Despite being clearly dead, his fingers occasionally twitch, and you swear'), nl,
    write('you can still hear faint whispering coming from his motionless lips.'), nl.

examine_item(pyramid_artifact) :-
    write('A small pyramid carved from obsidian-black stone. Its surface is covered with minute'), nl,
    write('carvings that seem to shift when not viewed directly. It feels unnaturally cold to the touch,'), nl,
    write('and somehow heavier than its size would suggest. This appears to match one of the recesses'), nl,
    write('in the statue base you saw in the main hall.'), nl.

examine_item(X) :-
    write('You see nothing special about the '), write(X), write('.'),
    nl.

take(X) :-
    member(X, [statue, pit, escape_attempt, priest, dead_priest]), 
    write('You cannot take that with you.'),
    nl, !.

take(pyramid_artifact) :-
    i_am_at(chapel),
    at(dead_priest, chapel),
    at(pyramid_artifact, chapel),
    retract(at(pyramid_artifact, chapel)),
    assert(inventory(pyramid_artifact, 1)),
    write('You take the stone pyramid from the altar, feeling its cold weight in your hand.'), nl,
    !.

take(pyramid_artifact) :-
    i_am_at(chapel),
    at(priest, chapel),
    at(pyramid_artifact, chapel),
    write('As you reach for the pyramid, the priest shrieks with rage and lunges at you.'), nl,
    (inventory(kitchen_knife, Qty), Qty > 0) ->
        write('In a moment of desperate reflexes, you draw your knife and defend yourself.'), nl,
        retract(at(pyramid_artifact, chapel)),
        retract(at(priest, chapel)),
        assert(at(dead_priest, chapel)),
        retract(inventory(kitchen_knife, _)),
        assert(inventory(pyramid_artifact, 1)),
        write('With shaking hands, you grab the stone pyramid from the altar.'), nl
    ;
        write('Defenseless, you can only shield yourself as the priest grasps your head.'), nl,
        write('He speaks words that burn into your mind. Your vision fractures,'), nl,
        write('and reality warps around you. You are now hallucinating.'), nl,
        assert(hallucinating),
        decrease_sanity(20)
    ,
    !.

take(_) :-
    decay_no_pickup,
    write('Your hands, ravaged by the spreading rot, are too weak to grasp anything.'), nl,
    write('The flesh crumbles and splits at the slightest pressure.'),
    nl, !.

take(X) :-
    i_am_at(Place),
    at(X, Place),
    retract(at(X, Place)),
    (inventory(X, Qty) -> 
        retract(inventory(X, Qty)),
        NewQty is Qty + 1,
        assert(inventory(X, NewQty))
    ;
        assert(inventory(X, 1))
    ),
    write('You pick up the '), write(X), write('.'),
    !, nl.

take(X) :-
    write('There is no '), write(X), write(' here to take.'),
    nl.

use(X) :-
    member(X, [dungeon_entrance_note]), 
    write('This item can only be examined, not used.'),
    nl, !.

use(X) :-
    inventory(X, Qty),
    Qty > 0,
    use_item(X),
    !.

use(X) :-
    write('You don''t have any '), write(X), write(' to use.'),
    nl.

use_item(torch) :- light_torch.

use_item(kitchen_knife) :-
    i_am_at(chapel),
    at(priest, chapel),
    write('You grip the kitchen knife tightly and approach the priest. His eyes widen in realization'), nl,
    write('but the smile never leaves his face. You plunge the blade into his chest. Black ichor,'), nl,
    write('not blood, spills from the wound as he collapses to the floor.'), nl,
    write('His body crumples, but the laughter continues for several seconds after.'), nl,
    retract(at(priest, chapel)),
    assert(at(dead_priest, chapel)),
    retract(inventory(kitchen_knife, _)),
    !.

use_item(kitchen_knife) :-
    write('You test the edge of the knife against your thumb. Still sharp.'), nl.

use_item(rotten_meat) :-
    consume_item(rotten_meat),
    decrease_hunger(15),
    decrease_sanity(10),
    write('You force yourself to eat the putrid flesh. The taste is foul beyond description,'), nl,
    write('and the texture makes you gag, but it does quiet the pangs of hunger somewhat.'), nl,
    write('Your mind recoils at what you''ve just consumed.'), nl.

use_item(fish) :-
    consume_item(fish),
    decrease_hunger(20),
    write('You tear into the dried fish. The taste is intensely salty and'), nl,
    write('the texture leathery, but it satisfies your hunger somewhat.'), nl.

use_item(bread) :- 
    consume_item(bread),
    decrease_hunger(25),
    write('You force down the stale bread. It''s nearly impossible to chew,'), nl,
    write('but your gnawing hunger subsides somewhat.'), nl.

use_item(wine_bottle) :-
    consume_item(wine_bottle),
    increase_sanity(15),
    increase_hunger(5),
    write('You pull the crumbling cork and drink deeply from the bottle.'), nl,
    write('The wine has turned to vinegar, but the alcohol still burns pleasantly,'), nl,
    write('temporarily dulling your awareness of the horrors surrounding you.'), nl.

use_item(opium_powder) :-
    consume_item(opium_powder),
    increase_sanity(20),
    retract(hallucinating),
    write('You consume some of the opium powder. A numbing calm washes over you,'), nl,
    write('dulling the terrors of the catacombs. Your mind feels more stable.'), nl.

use_item(blue_vial) :-
    consume_item(blue_vial),
    retract(sanity(_)),
    assert(sanity(100)),
    write('You drink the blue liquid. A wave of clarity washes over you as the catacombs'), nl,
    write('seem less threatening. Your mind feels restored and focused.'), nl.

use_item(green_vial) :-
    consume_item(green_vial),
    write('You drink the viscous green liquid, which immediately burns your throat.'), nl,
    write('To your horror, you feel it eating through your insides like acid.'), nl,
    write('Your body convulses as the corrosive substance dissolves you from within.'), nl,
    write('Your last sensation is that of your organs liquefying as darkness claims you.'), nl,
    halt.

use_item(purple_vial) :-
    consume_item(purple_vial),
    assert(hallucinating),
    write('You drink the purple liquid. The world around you fractures into impossible geometries.'), nl,
    write('Shadows dance at the edge of your vision, and whispers fill your ears.'), nl,
    write('Reality itself seems unstable. You fear this effect may be long-lasting.'), nl.

use_item(red_vial) :-
    consume_item(red_vial),
    assert(decaying_hands),
    assert(decay_turns(20)),
    write('You drink the crimson liquid. At first, nothing happens.'), nl,
    write('Then your hands begin to tingle unpleasantly. Looking down, you watch in horror'), nl,
    write('as your flesh begins to rot before your eyes. The decay is spreading slowly.'), nl,
    write('You must find a way to stop it before it consumes you entirely.'), nl.

use_item(X) :-
    consume_item(X),
    write('You use the '), write(X), write(', but nothing happens.'), nl.

consume_item(X) :-
    inventory(X, Qty),
    NewQty is Qty - 1,
    retract(inventory(X, Qty)),
    (NewQty > 0 -> 
        assert(inventory(X, NewQty))
    ;
        true
    ).

update_status :-
    increase_hunger(1),
    (hallucinating -> 
        decrease_sanity(4),
        hallucinate
    ;
        decrease_sanity(1)
    ),
    check_decaying_hands,
    check_game_state,
    update_torch.    

increase_hunger(Amount) :-
    hunger(Current),
    NewValue is min(100, Current + Amount),
    retract(hunger(Current)),
    assert(hunger(NewValue)),
    (NewValue >= 80 -> 
        write('Your stomach twists painfully with hunger.'), nl
    ; NewValue >= 50 ->
        write('You feel hungry.'), nl
    ; true).

decrease_hunger(Amount) :-
    hunger(Current),
    NewValue is max(0, Current - Amount),
    retract(hunger(Current)),
    assert(hunger(NewValue)).

increase_sanity(Amount) :-
    sanity(Current),
    NewValue is min(100, Current + Amount),
    retract(sanity(Current)),
    assert(sanity(NewValue)).

decrease_sanity(Amount) :-
    sanity(Current),
    torch_lit,
    NewValue is max(0, Current - Amount),
    retract(sanity(Current)),
    assert(sanity(NewValue)).

decrease_sanity(Amount) :-
    sanity(Current),
    NewValue is max(0, Current - (Amount * 2)),
    retract(sanity(Current)),
    assert(sanity(NewValue)).

check_game_state :-
    hunger(H),
    H >= 100, !,
    write('Overwhelming hunger causes you to collapse. Your body is too weak to continue.'), nl,
    halt.

check_game_state :-
    sanity(0), !,
    write('The darkness starts to consume your mind entirely. You collapse to the floor,'), nl,
    write('clawing at your own skin, desperate to escape the horrors in your mind.'), nl,
    write('Your journey ends here, lost in madness beneath the earth.'), nl,
    halt.

check_game_state :-
    sanity(S),
    S =< 30, S > 15,
    random(1, 5, R),    
    R =:= 1, !,        
    hallucinate.

check_game_state :-
    sanity(S),
    S =< 15, S > 0,
    random(1, 3, R),    
    R =:= 1, !,        
    hallucinate.

check_game_state.  

check_decaying_hands :-
    decaying_hands,
    decay_turns(T),
    T > 0,
    T1 is T - 1,
    retract(decay_turns(T)),
    assert(decay_turns(T1)),
    (T1 =:= 0 -> 
        write('The decay has consumed your arms entirely. Flesh sloughs from bone as'), nl,
        write('your limbs are reduced to twisted, useless appendages. The rot continues'), nl,
        write('to spread up throughout your body. Death comes slowly.'), nl,
        halt
    ; T1 < 5 ->
        write('The decay is nearly complete. Your limbs are barely recognizable.'), nl
    ; T1 < 10 ->
        assert(decay_no_pickup),
        write('Your hands are severely decayed. The rot is spreading up your arms.'), nl
    ; true),
        write('The decay continues to spread. You estimate approximately '), write(T1), 
        write(' turns remain before it consumes you entirely.'), nl.

check_decaying_hands.

hallucinate :-
    random(1, 6, R),
    hallucination(R).

hallucination(1) :-
    write('You hear whispering voices coming from the walls.'), nl.
hallucination(2) :-
    write('For a moment, you see blood dripping from the ceiling, but it vanishes when you blink.'), nl.
hallucination(3) :-
    write('Something skitters across your peripheral vision, but nothing is there when you turn.'), nl.
hallucination(4) :-
    write('You feel cold, invisible hands brushing against your face.'), nl.
hallucination(5) :-
    write('The ground beneath you seems to pulse, as if breathing.'), nl.

n :- go(n).
s :- go(s).
e :- go(e).
w :- go(w).
descend :- go(beneath).

go(Direction) :-
    i_am_at(Here),
    path(Here, Direction, There),
    retract(i_am_at(Here)),
    assert(i_am_at(There)),
    !, look,
    update_status.

go(_) :-
    write('You can''t go that way.').

look :-
    i_am_at(Place),
    describe(Place),
    nl,
    notice_objects_at(Place).

notice_objects_at(Place) :-
    at(X, Place),
    write('There is a '), write(X), write(' here.'), nl,
    fail.

notice_objects_at(_).

light_torch :-
    \+ torch_lit,
    consume_item(torch),
    assert(torch_lit),
    assert(torch_remaining(20)),
    write('You light the torch. The darkness recedes as flickering flames cast long shadows on the ancient walls.'),
    nl, !.

light_torch :-
    torch_lit,
    write('You already have a lit torch.'),
    nl, !.

light_torch :-
    (\+ inventory(torch, _); inventory(torch, 0)),
    write('You don\'t have a torch to light.'),
    nl.

update_torch :-
    torch_lit,
    torch_remaining(T),
    T > 1,
    T1 is T - 1,
    retract(torch_remaining(T)),
    assert(torch_remaining(T1)),
    nl,
    write('Your torch burns dimmer. '), write(T1), write(' turns remaining.'),
    nl.

update_torch :-
    torch_lit,
    torch_remaining(1),
    retract(torch_remaining(1)),
    assert(torch_remaining(0)),
    retract(torch_lit),
    nl,
    write('Your torch flickers and dies, plunging you into darkness.'),
    nl.

update_torch.

info :-
    write('Status:'), nl,
    write('Sanity: '), sanity(S), write(S), write('%'), nl,
    write('Hunger: '), hunger(H), write(H), write('%'), nl,
    nl,
    write('Inventory:'), nl,
    list_inventory.
    
list_inventory :-
    inventory(Item, Qty),
    Qty > 0,
    write('  - '), write(Item), write(' ('), write(Qty), write(')'), nl,
    fail.
    
list_inventory :-
    \+ (inventory(_, Qty), Qty > 0),
    write('  Nothing.'), nl.

list_inventory.

instructions :-
    nl,
    write('Enter commands using standard Prolog syntax.'), nl,
    write('Available commands are:'), nl,
    write('start.             -- to start the game.'), nl,
    write('n.  s.  e.  w.  beneath.     -- to go in that direction.'), nl,
    write('take(Object).      -- to pick up an object.'), nl,
    write('examine(Object).   -- to display info about an item.'), nl,
    write('use(Object).       -- to use an object.'), nl,
    write('look.              -- to look around you again.'), nl,
    write('info.              -- to display info about your inventory and status.'), nl,
    write('instructions.      -- to see this message again.'), nl,
    write('halt.              -- to end the game and quit.'), nl,
    nl.

intro :-
    nl,
    write('The year is 1433. A bitter wind carries the scent of decay as darkness falls over the land.'), nl,
    write('Sir Galahad, once revered as the most valiant of the king''s knights, was dispatched to these'), nl,
    write('accursed catacombs with a desperate mission - to retrieve the Chalice of Immaculate Tears,'), nl,
    write('the only hope for our monarch who writhes in agony as black veins spread beneath his skin.'), nl,
    write('Three moons have waxed and waned since Sir Galahad departed into these forsaken lands.'), nl,
    write('As his loyal squire, you now stand where he once stood, the weight of duty like a millstone.'), nl,
    write('The locals speak only in whispers of what dwells below - abominations born of forbidden alchemy,'), nl,
    write('tortured souls that wander the eternal darkness, and horrors that feast upon sanity itself.'), nl,
    write('Hunger and fear will be your constant companions in this labyrinth of the damned.'), nl, nl.

start :-
    init_game,
    intro,
    instructions,
    look.

describe(dungeon_entrance) :-
    write('You are at the entrance to the ancient catacombs. The stone steps lead down into darkness.'), nl,
    write('A cold draft whispers from below, carrying with it the scent of old stone and something else...'), nl.

describe(main_hall) :-
    write('You stand in the vast main hall of the catacombs.'), nl,
    write('The vaulted ceiling vanishes into darkness, as if swallowed by the void itself.'), nl,
    write('Stone pillars rise like calcified trees, each carved with symbols that seem to writhe'), nl,
    write('when viewed from the corner of your eye. Blood-red lichen clings to the damp walls.'), nl,
    write('At the center looms a weathered statue of some ancient deity - part human, part beast -'), nl,
    write('its hollow eyes seeming to track your movement. A viscous substance drips from its mouth.'), nl,
    write('Three archways lead into darkness: north, east, and west. From somewhere deep below,'), nl,
    write('you hear the faint sound of something that might be weeping... or laughter.'), nl.

describe(dark_room1) :-
    write('You enter a small, rectangular chamber with crumbling stone walls. in the center of'), nl,
    write('the floor gapes a dark, circular pit. a putrid stench'), nl,
    write('rises from the abyss, making your eyes water and stomach churn.'), nl,
    write('The darkness below seems absolute, swallowing the feeble light completely.'), nl,
    write('Something about the way sound echoes when small debris falls in suggests'), nl,
    write('a terrifying depth. The rim of the hole is worn smooth.'), nl,
    write('Dark stains radiate outward from the pit. Scratches cover the nearby walls,'), nl,
    write('some appearing to form words now too faded to read.'), nl.

describe(pit_bottom) :-
    write('You are submerged waist-deep in foul liquid at the bottom of the hole.'), nl,
    write('The walls rise impossibly high above, the entrance now just a distant pinprick of light.'), nl,
    (torch_lit -> 
        write('Your torch struggles against the oppressive darkness of this place.'), nl
    ;
        write('The darkness here is absolute, suffocating.'), nl
    ).

describe(alchemy_lab) :-
    write('You enter an ancient alchemical workshop. Heavy stone tables are stained with centuries of spilled reagents.'), nl,
    write('Cracked retorts, dusty alembics, and rusted instruments lie scattered across the workspace.'), nl,
    write('Shelves line the walls, bearing clay jars with faded labels and the desiccated remains of strange herbs.'), nl,
    write('In the center of the room, a curious apparatus of brass tubes and glass bulbs stands cold and silent.'), nl,
    write('Among the debris on the main table, you notice four small vials of different colors: blue, green, purple, and red.'), nl.

describe(fork1) :-
    write('You stand at a grim intersection where three pathways meet.'), nl,
    write('The air is stagnant here, heavy with the scent of mold and decay.'), nl,
    write('Carved into the weathered stone walls are strange symbols - warnings, perhaps,'), nl,
    write('or invocations to forgotten deities that once held sway in these depths.'), nl,
    write('To the west, an unfamiliar odor wafts from the darkness.'), nl,
    write('Eastward, you hear distant, rhythmic chanting that raises the hair on your neck.'), nl,
    write('The floor is marked with countless scratches, as if something was dragged through here repeatedly.'), nl.

describe(kitchen) :-
    write('You enter what once served as a kitchen for the inhabitants of these catacombs.'), nl,
    write('A massive stone hearth dominates one wall, its interior blackened with ancient soot.'), nl,
    write('Long wooden tables, warped and split with age, line the center of the room.'), nl,
    write('Rusted implements hang from hooks on the walls, and cracked ceramic vessels'), nl,
    write('lie scattered across shelves and in corners. The air carries a sickly-sweet smell'), nl,
    write('of spoiled food mingled with something less identifiable. Dark stains cover'), nl,
    write('the large butchering block in the corner. Something scurries away as you enter.'), nl.

describe(chapel) :-
    at(priest, chapel),
    write('You enter a small chapel carved into the living rock. The air is thick with incense'), nl,
    write('and the sickly-sweet smell of decay. Rows of broken pews face a blackened stone altar,'), nl,
    write('upon which rest strange artifacts including a small stone pyramid. Dozens of candles cast'), nl,
    write('dancing shadows across wall carvings depicting scenes of sacrifice and transformation.'), nl,
    write('At the altar stands a figure in tattered robes, facing away from you, muttering'), nl,
    write('in a language you do not understand. At the sound of your entrance, the figure'), nl,
    write('turns slowly, revealing a gaunt face with sunken eyes that gleam with madness.'), nl,
    write('The priest''s lips curl into a terrible smile.'), nl,
    !.

describe(chapel) :-
    at(dead_priest, chapel),
    write('You enter the defiled chapel, now eerily silent except for the dripping of black ichor'), nl,
    write('from the altar. The stone pyramid still rests among the ritual items. Candles burn with'), nl, 
    write('strange blue flames, casting grotesque shadows of the dead priest''s body across the walls.'), nl,
    write('The air feels heavier, as if the ritual space has been further corrupted by the violent death.'), nl,
    write('A low, almost subsonic humming emanates from somewhere beneath the altar.'), nl,
    !.
