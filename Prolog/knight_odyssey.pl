:- dynamic i_am_at/1, at/2, path/3, inventory/2, sanity/1, hunger/1, torch_lit/0, torch_remaining/1, decay_no_pickup/0, hallucinating/0, decaying_hands/0, decay_turns/1, artifacts_placed/1.

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
    retractall(artifacts_placed(_)),
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
    assert(at(torch, fork1)),
    assert(at(kitchen_knife, kitchen)),
    assert(at(rotten_meat, kitchen)),
    assert(at(fish, kitchen)),
    assert(at(bread, kitchen)),
    assert(at(wine_bottle, kitchen)),
    assert(at(priest, chapel)),
    assert(at(pyramid_artifact, chapel)),
    assert(at(alchemy_techniques, library)),
    assert(at(merthveer, library)),
    assert(at(silent_figure, jail)),
    assert(at(human_remains, jail)),
    assert(at(horrified_man, jail)),
    assert(at(dying_prisoner, torture_chamber)),
    assert(at(corroded_dagger, torture_chamber)),
    assert(at(sleeping_guard, guard_quarters)),
    assert(at(whisky, guard_quarters)),
    assert(at(vodka, guard_quarters)),
    assert(at(cube_artifact, crypt)),
    assert(path(dungeon_entrance, n, main_hall)),
    assert(path(main_hall, s, dungeon_entrance)),
    assert(path(main_hall, e, dark_room1)),
    assert(path(dark_room1, beneath, pit_bottom)),
    assert(path(dark_room1, w, main_hall)),
    assert(path(main_hall, w, alchemy_lab)),
    assert(path(alchemy_lab, e, main_hall)),
    assert(path(main_hall, n, fork1)),
    assert(path(fork1, s, main_hall)),
    assert(path(fork1, w, kitchen)),
    assert(path(kitchen, e, fork1)),
    assert(path(fork1, e, chapel)),
    assert(path(chapel, w, fork1)),
    assert(path(chapel, s, library)),
    assert(path(library, n, chapel)),
    assert(path(fork1, n, fork2)),
    assert(path(fork2, s, fork1)),
    assert(path(fork2, w, guard_quarters)),
    assert(path(guard_quarters, e, fork2)),
    assert(path(fork2, e, jail)),
    assert(path(jail, w, fork2)),
    assert(path(jail, n, torture_chamber)),
    assert(path(torture_chamber, s, jail)),
    assert(path(fork2, n, crypt)),
    assert(path(crypt, s, fork2)),
    assert(sanity(100)),
    assert(hunger(0)),
    assert(artifacts_placed(0)).


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
    write('You''ve never encountered anything resembling this entity in any religious text or traveler''s tale.'), nl,
    write('The base of the statue contains three distinct recesses, shaped to hold three specific objects:'), nl,
    write('- a cube'), nl,
    write('- a pyramid'), nl,
    write('- a sphere'), nl,
    write('Looking closer, you notice scrape marks on the stone floor around the statue''s base,'), nl,
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
    write('The priest''s corpse lies in a pool of black fluid that seems to move slightly of its own accord.'), nl,
    write('His face is frozen in that terrible smile, eyes still open and staring at nothing.'), nl,
    write('The wound in his chest pulses faintly, as if something inside is still alive.'), nl,
    write('Despite being clearly dead, his fingers occasionally twitch, and you swear'), nl,
    write('you can still hear faint whispering coming from his motionless lips.'), nl.

examine_item(pyramid_artifact) :-
    write('A small pyramid carved from obsidian-black stone. Its surface is covered with minute'), nl,
    write('carvings that seem to shift when not viewed directly. It feels unnaturally cold to the touch,'), nl,
    write('and somehow heavier than its size would suggest. This appears to match one of the recesses'), nl,
    write('in the statue base you saw in the main hall.'), nl.

examine_item(sphere_artifact) :-
    write('A perfect sphere of obsidian-black stone. No tool marks are visible on its flawless surface.'), nl,
    write('It feels unnaturally cold to the touch, and somehow heavier than its size would suggest.'), nl,
    write('When you look closely, you see pinpricks of light deep within, like distant stars.'), nl,
    write('This appears to match one of the recesses in the statue base you saw in the main hall.'), nl.

examine_item(cube_artifact) :-
    write('A perfect cube of obsidian-black stone. When you touch it, visions of unspeakable'), nl,
    write('horror flood your mind - people transformed into twisted amalgamations of flesh and metal,'), nl,
    write('rituals performed on living subjects whose screams echo through eternity.'), nl,
    write('It feels unnaturally cold to the touch, and somehow heavier than its size would suggest.'), nl,
    write('This appears to match one of the recesses in the statue base you saw in the main hall.'), nl,
    decrease_sanity(5).

examine_item(alchemy_techniques) :-
    write('A weathered tome with a cracked leather spine. The pages detail ancient alchemical processes.'), nl,
    write('You find several relevant sections:'), nl,
    write('- Opium powder: "A salve for the troubled mind. Calms the nerves and dispels phantasms."'), nl,
    write('- Blue vial: "Essence of clarity. Restores the mind to perfect equilibrium."'), nl,
    write('- Green vial: "The universal solvent. Dissolves flesh, bone, and even the hardest armor with ease."'), nl,
    write('- Purple vial: "Opens the third eye to visions beyond mortal comprehension."'), nl,
    write('- Red vial: "The curse of living decay. Flesh rots while consciousness remains."'), nl.

examine_item(merthveer) :-
    write('A book bound in what appears to be human skin. The title "Merthveer" is branded onto the cover.'), nl,
    write('The pages contain disturbing illustrations of rituals performed before a many-limbed deity'), nl,
    write('with multiple eyes arranged in an unnatural pattern.'), nl,
    write('The text describes an ancient being that exists beyond human comprehension, whose mere'), nl,
    write('presence corrupts the mind and warps reality.'), nl,
    write('Several pages are stained with substances you prefer not to identify.'), nl,
    write('Reading further makes your vision swim and your sanity waver.'), nl,
    decrease_sanity(10).

examine_item(silent_figure) :-
    write('You approach the bars to get a better look at the silent prisoner.'), nl,
    write('His skin is unnaturally pale, stretched too tightly over sharp bones.'), nl,
    write('His eyes never blink as they follow your every movement.'), nl,
    write('His lips are sewn shut with crude black thread, yet you swear'), nl,
    write('you can hear faint whispers emanating from him.'), nl,
    write('As you turn to leave, you notice his shadow doesn''t match his body''s position.'), nl,
    decrease_sanity(5).

examine_item(human_remains) :-
    write('The corpse has been here for some time, though the dry air has preserved it somewhat.'), nl,
    write('The body is contorted in an unnatural position, suggesting a violent end.'), nl,
    write('The skull has been opened and appears hollow, as if its contents were removed.'), nl,
    write('There are strange symbols carved into the leathery skin across the chest.'), nl,
    write('Several fingers and toes appear to have been systematically removed, the wounds cauterized.'), nl,
    write('A small object glints faintly in the ribcage, partially hidden by the withered organs.'), nl.

examine_item(horrified_man) :-
    write('The man cowers in the far corner of his cell as you approach.'), nl,
    write('His once-fine clothes hang in tatters, and his body bears countless small, precise cuts.'), nl,
    write('His eyes are wide with terror as he whispers urgently: "They take us one by one...'), nl,
    write('down that passage... we hear the screams... then nothing. The guard..."'), nl,
    write('He clutches something small and metallic in his trembling hands, hiding it'), nl,
    write('whenever he hears a sound from the passage. "I found a beautiful sphere in this place, black as night.'), nl,
    write('That thing... the guard... took it from me. Said it belonged to his master..."'), nl.

examine_item(dying_prisoner) :-
    write('You approach the hanging man, whose breaths come in shallow, wet gasps.'), nl,
    write('His body has been systematically mutilated - skin peeled away in precise patterns'), nl,
    write('that form symbols similar to those you''ve seen elsewhere in the catacombs.'), nl,
    write('His jaw has been removed entirely, and his tongue cut out, ensuring his silence.'), nl,
    write('One eye has been surgically extracted.'), nl,
    write('A small key on a chain hangs from a nail driven through his ankle.'), nl.

examine_item(corroded_dagger) :-
    write('A dagger with a blade corroded by time and strange fluids.'), nl,
    write('Despite its deteriorated condition, it maintains a wicked edge.'), nl,
    write('Strange symbols are etched into the blade, visible despite the corrosion.'), nl,
    write('It feels unnaturally cold to the touch, and somehow heavier than it should be.'), nl.

examine_item(sleeping_guard) :-
    write('As you approach cautiously, you realize with horror that this is not armor at all.'), nl,
    write('The metal plates are fused directly to the creature''s flesh - grown from it, part of it.'), nl,
    write('Its proportions are wrong - limbs too long, joints bending in impossible directions.'), nl,
    write('Each breath it takes produces a sound like metal grinding against wet stone.'), nl,
    write('Its helm-like head has no visor, only a solid plate with crude eyeholes that leak a yellow fluid.'), nl,
    write('One massive gauntleted hand clutches an iron mace even in sleep.'), nl,
    write('You pray it doesn''t wake.'), nl.

examine_item(whisky) :-
    write('A medium size bottle of amber liquid. The label has long since rotted away.'), nl,
    write('The liquid inside still sloshes invitingly, preserved by the high alcohol content.'), nl,
    write('Such spirits might numb your mind to the horrors of this place, at least temporarily.'), nl.

examine_item(vodka) :-
    write('A clear liquid in a dusty bottle. Despite its age, it appears unspoiled.'), nl,
    write('The strong smell of alcohol burns your nostrils when you remove the cork.'), nl,
    write('Drinking this might provide temporary relief for your fraying nerves.'), nl.


take(X) :-
    member(X, [statue, pit, escape_attempt, priest, dead_priest, silent_figure, human_remains, horrified_man, dying_prisoner, sleeping_guard]), 
    write('You cannot take that with you.'),
    nl, !.

take(_) :-
    decay_no_pickup,
    write('Your hands, ravaged by the spreading rot, are too weak to grasp anything.'), nl,
    write('The flesh crumbles and splits at the slightest pressure.'),
    nl, !.

take(pyramid_artifact) :-
    i_am_at(chapel),
    at(dead_priest, chapel),
    at(pyramid_artifact, chapel),
    retract(at(pyramid_artifact, chapel)),
    assert(inventory(pyramid_artifact, 1)),
    write('You take the stone pyramid from the altar, feeling its cold weight in your hand.'), nl, !.

take(pyramid_artifact) :-
    i_am_at(chapel),
    at(priest, chapel),
    at(pyramid_artifact, chapel),
    write('As you reach for the pyramid, the priest shrieks with rage and lunges at you.'), nl,
    ((inventory(kitchen_knife, Qty), Qty > 0) ->
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
    ).

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
    nl, !.

take(X) :-
    write('There is no '), write(X), write(' here to take.'),
    nl.

use(X) :-
    member(X, [dungeon_entrance_note, alchemy_techniques, merthveer]), 
    write('This item can only be examined, not used.'),
    nl, !.

use(X) :-
    inventory(X, Qty),
    Qty > 0,
    use_item(X), !.

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
    i_am_at(guard_quarters),
    at(sleeping_guard, guard_quarters),
    write('You carefully unstop the vial and throw the viscous green liquid at the sleeping guard.'), nl,
    write('The acid splashes across its metal-fused flesh, instantly hissing and bubbling.'), nl,
    write('The creature awakens with a horrific shriek as the corrosive substance eats through'), nl,
    write('its armored plates, revealing pulsing, inhuman flesh beneath.'), nl,
    ((inventory(corroded_dagger, Qty), Qty > 0) ->
        write('Acting on instinct, you lunge forward with your dagger, driving it deep into'), nl,
        write('the exposed flesh of its throat. Black ichor sprays from the wound as the creature'), nl,
        write('thrashes wildly before collapsing in a heap. Its body continues to twitch'), nl,
        write('as the acid dissolves more of its form.'), nl,
        write('As its body deteriorates, a perfect black sphere rolls from its remains.'), nl,
        retract(at(sleeping_guard, guard_quarters)),
        assert(at(dead_guard, guard_quarters)),
        assert(at(sphere_artifact, guard_quarters))
    ;
        write('The creature rises in agony and rage, its faceplate splitting open to reveal'), nl,
        write('rows of needle-like teeth. Though wounded and burning, it hefts its massive'), nl,
        write('halberd with terrifying speed. The last thing you see is the blade arcing'), nl,
        write('toward your neck before darkness claims you.'), nl,
        halt
    ), !.

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

use_item(whisky) :-
    consume_item(whisky),
    increase_sanity(25),
    increase_hunger(10),
    write('You take a long swig of the aged whisky. It burns going down,'), nl,
    write('but spreads a comforting warmth through your body.'), nl,
    write('The horrors around you seem slightly more distant now.'), nl.

use_item(vodka) :-
    consume_item(vodka),
    increase_sanity(25),
    increase_hunger(10),
    write('You drink the strong, clear spirit. It sears your throat'), nl,
    write('but brings a numbing calm to your mind.'), nl,
    write('Your fear recedes temporarily as the alcohol dulls your senses.'), nl.

use_item(corroded_dagger) :-
    i_am_at(guard_quarters),
    at(sleeping_guard, guard_quarters),
    write('You approach the sleeping armored figure, dagger raised to strike a killing blow.'), nl,
    write('The blade connects with the metallic plating but merely scrapes across it with a hideous screech.'), nl,
    write('The creature''s eyes snap open - glowing yellow orbs behind the metal faceplate.'), nl,
    write('It rises with impossible speed, a massive gauntlet seizing your throat.'), nl,
    write('Your dagger clatters uselessly to the floor as your feet leave the ground.'), nl,
    write('The last thing you see is its faceplate splitting open vertically, revealing rows'), nl,
    write('of needle-like teeth before darkness claims you...'), nl,
    write('You awaken briefly in the torture chamber, strapped to a table as the creature'), nl,
    write('methodically begins its work. The pain is beyond comprehension.'), nl,
    write('Death, when it finally comes, is a mercy.'), nl,
    halt.

use_item(corroded_dagger) :-
    write('You test the edge of the dagger. Despite its corrosion, it remains surprisingly sharp.'), nl.

use_item(pyramid_artifact) :-
    i_am_at(main_hall),
    consume_item(pyramid_artifact),
    write('You carefully place the pyramid artifact into the matching recess in the statue base.'), nl,
    write('It fits perfectly, and you hear a faint click as it locks into place.'), nl,
    write('The air grows colder, and whispers fill your mind momentarily. The statue''s eyes briefly glow red.'), nl,
    increase_placed_artifacts,
    check_all_artifacts_placed, !.

use_item(sphere_artifact) :-
    i_am_at(main_hall),
    consume_item(sphere_artifact),
    write('You carefully place the sphere artifact into the matching recess in the statue base.'), nl,
    write('The sphere settles with an ominous hum, and you hear a faint click as it locks into place.'), nl,
    write('The ground beneath you trembles slightly. You notice drops of a black, viscous liquid'), nl,
    write('beginning to seep from the statue''s mouth.'), nl,
    increase_placed_artifacts,
    check_all_artifacts_placed, !.

use_item(cube_artifact) :-
    i_am_at(main_hall),
    consume_item(cube_artifact),
    write('You carefully place the cube artifact into the matching recess in the statue base.'), nl,
    write('As the cube slots into place, the entire chamber resonates with a deep, subsonic tone.'), nl,
    write('Shadows in the room appear to elongate, stretching toward the statue. You feel an'), nl,
    write('uncomfortable pressure in your head, as if something is probing your thoughts.'), nl,
    decrease_sanity(10),
    increase_placed_artifacts,
    check_all_artifacts_placed, !.

use_item(pyramid_artifact) :-
    write('This artifact seems to match one of the recesses in the statue in the main hall.'), nl.

use_item(sphere_artifact) :-
    write('This artifact seems to match one of the recesses in the statue in the main hall.'), nl.

use_item(cube_artifact) :-
    write('This artifact seems to match one of the recesses in the statue in the main hall.'), nl.

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

increase_placed_artifacts :-
    artifacts_placed(Count),
    NewCount is Count + 1,
    retract(artifacts_placed(Count)),
    assert(artifacts_placed(NewCount)).

check_all_artifacts_placed :-
    artifacts_placed(3),
    write('As the final artifact is placed, the statue begins to tremble violently.'), nl,
    write('The eyes of the deity blaze with unholy light, and the mouth opens wider, disgorging'), nl,
    write('a torrent of black ichor that pools around the base.'), nl,
    write('With a grinding sound of stone against stone, the floor beneath the statue'), nl,
    write('splits open, revealing an impossibly narrow, spiral staircase carved into the bedrock.'), nl,
    write('The steps are worn and treacherously steep, descending at an unnatural angle into absolute darkness.'), nl,
    write('A cold draft rises from below, carrying whispers in a language you cannot comprehend.'), nl,
    write('The descent appears hazardous, but now accessible.'), nl,
    assert(path(main_hall, beneath, inner_sanctum)), !.

check_all_artifacts_placed.

update_status :-
    (hallucinating -> 
        decrease_sanity(4),
        hallucinate
    ;
        decrease_sanity(1)
    ),
    check_decaying_hands,
    check_game_state,
    increase_hunger(3),
    update_torch, !.

increase_hunger(Amount) :-
    hunger(Current),
    NewValue is min(100, Current + Amount),
    retract(hunger(Current)),
    assert(hunger(NewValue)),
    (NewValue >= 80 -> 
        nl,
        write('Your stomach twists painfully with hunger.'), nl
    ; NewValue >= 50 ->
        nl,
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
    nl,
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
    nl,
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

update_torch :-
    torch_lit,
    torch_remaining(T),
    T > 1,
    T1 is T - 1,
    retract(torch_remaining(T)),
    assert(torch_remaining(T1)),
    nl,
    write('Your torch burns dimmer. '), write(T1), write(' turns remaining.'),
    nl, !.

update_torch :-
    torch_lit,
    torch_remaining(1),
    retract(torch_remaining(1)),
    assert(torch_remaining(0)),
    retract(torch_lit),
    nl,
    write('Your torch flickers and dies, plunging you into darkness.'),
    nl, !.

update_torch.

status :-
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

help :-
    nl,
    write('Enter commands using standard Prolog syntax.'), nl,
    write('Available commands are:'), nl,
    write('n.  s.  e.  w.  descend.     -- to go in that direction.'), nl,
    write('take(Object).                -- to pick up an object.'), nl,
    write('examine(Object).             -- to display info about an item.'), nl,
    write('use(Object).                 -- to use an object.'), nl,
    write('look.                        -- to look around you again.'), nl,
    write('status.                      -- to display info about your inventory and status.'), nl,
    write('help.                        -- to see this message again.'), nl,
    write('halt.                        -- to end the game and quit.'), nl,
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
    help,
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
    write('You stand at a grim intersection where multiple pathways meet.'), nl,
    write('The air is stagnant here, heavy with the scent of mold and decay.'), nl,
    write('Carved into the weathered stone walls are strange symbols - warnings, perhaps,'), nl,
    write('or invocations to forgotten deities that once held sway in these depths.'), nl,
    write('To the west, an unfamiliar odor wafts from the darkness.'), nl,
    write('Eastward, you hear distant, rhythmic chanting that raises the hair on your neck.'), nl,
    write('Northward, the passage descends deeper, and a trail of dark, congealed blood disappears into the shadows.'), nl,
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

describe(library) :-
    write('You enter a library filled with dust-covered tomes and scrolls.'), nl,
    write('Towering bookshelves stretch toward the shadowed ceiling, some tilting precariously under the weight of knowledge.'), nl,
    write('Parchment scrolls and leather-bound volumes lie scattered across tables and the floor.'), nl,
    write('Many books are bound in materials that don''t appear to be ordinary leather.'), nl,
    write('Strange symbols have been scratched into the stone floor, forming a pattern that seems'), nl,
    write('to converge at the center of the room where a reading podium stands.'), nl,
    write('Some sections of the bookshelves appear burned, and you notice dark stains trailing'), nl,
    write('from several tomes as if they had bled.'), nl,
    write('The air feels heavy with forgotten wisdom and forbidden knowledge.'), nl.

describe(fork2) :-
    write('You reach another grim intersection, deeper into the catacombs.'), nl,
    write('The air here is thick with the scent of damp stone and something faintly metallic.'), nl,
    write('From the western passage, you hear heavy, rhythmic breathing - something large slumbers there.'), nl,
    write('To the east, faint whimpers and the distant clink of chains echo through the darkness.'), nl,
    write('The northern corridor slopes gently downward, carrying the scent of ancient death.'), nl,
    write('The walls here bear marks of violence - deep gouges in the stone and what appear to be'), nl,
    write('handprints made in dried blood. The trail continues here growing fresher'), nl,
    write('and more abundant as it disappears down the eastern passage.'), nl.

describe(jail) :-
    write('You enter a dank prison chamber carved from the living stone.'), nl,
    write('Three cells line the far wall, their rusted iron bars casting skeletal shadows in your light.'), nl,
    write('In the first cell, a gaunt figure sits motionless in the corner, eyes reflecting your light'), nl,
    write('like those of a nocturnal beast. He makes no sound, but never breaks his stare.'), nl,
    write('The middle cell contains the withered corpse of what might have once been a man,'), nl,
    write('now little more than leathery skin stretched over a crooked frame.'), nl,
    write('From the third cell, a trembling voice whispers: "Please... help me... He... This thing...'), nl,
    write('what he did to the others... oh gods... please..."'), nl,
    write('The trail of blood leads to a dark passage on the northern wall,'), nl,
    write('disappearing into shadows so thick they seem to devour your light.'), nl,
    write('Countless handprints in dried blood mark the stone around the entrance.'), nl.

describe(torture_chamber) :-
    write('You enter a chamber of horrors. The air reeks of blood, excrement, and fear.'), nl,
    write('In the center of the room, suspended from the ceiling by rusted chains,'), nl,
    write('hangs what was once a man. His body is a canvas of methodical mutilation,'), nl,
    write('skin flayed in precise patterns. Blood still drips slowly from his wounds,'), nl,
    write('forming a dark pool beneath him. His eyes are open but unseeing - death approaches,'), nl,
    write('but has not yet granted mercy.'), nl,
    write('Against the far wall stands a weapons rack containing a corroded sword.'), nl.

describe(guard_quarters) :-
    at(sleeping_guard, guard_quarters),
    write('You enter what appears to be living quarters for whatever guards this forsaken place.'), nl,
    write('Several bunks line the walls, most collapsed with age and decay.'), nl,
    write('A wooden cabinet against one wall holds numerous bottles of spirits, some still sealed.'), nl,
    write('In the center of the room, sprawled across a massive iron-framed bed, lies what first appears'), nl,
    write('to be a guard in enormous plate armor.'), nl,
    !.

describe(guard_quarters) :-
    at(dead_guard, guard_quarters),
    write('The guard quarters are now dominated by the fallen form of the mutated guard.'), nl,
    write('Its body continues to dissolve where the acid made contact, revealing a disturbing'), nl,
    write('amalgamation of metal, flesh, and something else entirely beneath the armor.'), nl,
    write('Black ichor pools around the corpse, bubbling occasionally as if still alive.'), nl,
    write('Several bunks line the walls, most collapsed with age and decay.'), nl,
    write('The air feels lighter somehow, as if a malevolent presence has been removed.'), nl,
    !.

describe(crypt) :-
    write('You enter an ancient crypt, the air thick with the stench of death and decay.'), nl,
    write('Stone sarcophagi line the walls, their carved lids depicting the tortured faces of those within.'), nl,
    write('Most have been desecrated, broken open by force, with desiccated remains spilling onto the floor.'), nl,
    write('In the center of the chamber stands a raised altar of black stone, its surface stained with ancient blood.'), nl,
    write('Upon it rests a perfect cube of obsidian-black stone.'), nl,
    write('From somewhere within the sealed sarcophagi, you hear faint scratching sounds, as if something is trying to get out.'), nl,
    write('Whispers fill your mind when you focus on the obsidian cube, promising terrible knowledge and power.'), nl,
    write('The floor beneath your feet feels unnaturally warm, and the shadows in the corners seem to move of their own accord.'), nl.

describe(inner_sanctum) :-
    write('You carefully navigate the treacherous spiral staircase, your hands pressed against'), nl,
    write('the damp stone walls for balance. The steps are unnaturally steep and narrow,'), nl,
    write('forcing you to descend sideways while hunched over.'), nl,
    write('After what seems like an eternity of careful descent, the staircase opens into a vast chamber.'), nl,
    write('Your torch illuminates a horrific scene before you. The chamber is vast, its ceiling lost to darkness.'), nl,
    write('Massive columns rise like petrified trees, carved with scenes of torture and transformation.'), nl,
    write('The floor is slick with black ichor that pulses with subtle movement.'), nl,
    write('And there, sprawled before an obsidian altar, lies the broken body of Sir Galahad.'), nl,
    write('His once-proud armor is shattered, the metal twisted and fused with his flesh in places.'), nl,
    write('His face, frozen in an expression of absolute horror, bears signs of transformation - '), nl,
    write('skin stretched too tight over elongated features, eyes sunken yet somehow too large.'), nl,
    write('Clutched in his deformed hand is the Chalice of Immaculate Tears, its crystalline surface'), nl,
    write('glimmering with unholy light. The liquid within moves against gravity, occasionally forming'), nl,
    write('shapes that resemble faces screaming in agony.'), nl,
    write('As you approach, Sir Galahad''s corpse twitches. His head turns with a sickening crack,'), nl,
    write('dead eyes fixing on yours. His jaw drops, dislocating with the movement, and'), nl,
    write('a voice that is not his whispers: "You are too late, squire. The king is already ours."'), nl,
    write('Darkness claims your vision as clawed hands grasp you from behind.'), nl,
    write('Some things are not meant to be found. Some quests are not meant to succeed.'), nl,
    write('Your story ends here, in the darkness beneath the earth, joining the countless others'), nl,
    write('who came seeking power, salvation, or knowledge.'), nl,
    write('THE END'), nl,
    halt.
