Game extend [

    initializeWorld [
        | dungeonEntrance mainHall darkRoom1 pitBottom alchemyLab fork1 kitchen chapel library fork2 guardQuarters jail tortureChamber crypt innerSanctum item |

        "--- Tworzenie i podstawowa konfiguracja lokacji ---"
        dungeonEntrance := Location new.
        dungeonEntrance name: 'dungeon_entrance'.
        dungeonEntrance description: [
            #(
                'You are at the entrance to the ancient catacombs. The stone steps lead down into darkness.'
                'A cold draft whispers from below, carrying with it the scent of old stone and something else...'
            )
        ].
        Smalltalk at: #dungeon_entrance put: dungeonEntrance.

        "main_hall jest teraz typu StatueRoom"
        mainHall := StatueRoom new. "Używamy nowej klasy StatueRoom"
        mainHall name: 'main_hall'.
        mainHall description: [
            #(
                'You stand in the vast main hall of the catacombs.'
                'The vaulted ceiling vanishes into darkness, as if swallowed by the void itself.'
                'Stone pillars rise like calcified trees, each carved with symbols that seem to writhe'
                'when viewed from the corner of your eye. Blood-red lichen clings to the damp walls.'
                'At the center looms a weathered statue of some ancient deity - part human, part beast -'
                'its hollow eyes seeming to track your movement. A viscous substance drips from its mouth.'
                'Three archways lead into darkness: north, east, and west. From somewhere deep below,'
                'you hear the faint sound of something that might be weeping... or laughter.'
            )
        ].
        "Dodatkowa konfiguracja dla StatueRoom, jeśli wartości domyślne nie pasują:"
        "mainHall maxArtifacts: 3."
        "mainHall pathNameToInnerSanctum: 'beneath.'."
        "mainHall innerSanctumLocationName: #inner_sanctum."
        Smalltalk at: #main_hall put: mainHall.

        darkRoom1 := Location new.
        darkRoom1 name: 'dark_room1'.
        darkRoom1 description: [
            #(
                'You enter a small, rectangular chamber with crumbling stone walls. In the center of'
                'the floor gapes a dark, circular pit. A putrid stench'
                'rises from the abyss, making your eyes water and stomach churn.'
                'The darkness below seems absolute, swallowing the feeble light completely.'
                'Something about the way sound echoes when small debris falls in suggests'
                'a terrifying depth. The rim of the hole is worn smooth.'
                'Dark stains radiate outward from the pit. Scratches cover the nearby walls,'
                'some appearing to form words now too faded to read.'
            )
        ].
        Smalltalk at: #dark_room1 put: darkRoom1.

        pitBottom := Location new.
        pitBottom name: 'pit_bottom'.
        pitBottom description: [
            | descriptionParts |
            descriptionParts := #(
                'You are submerged waist-deep in foul liquid at the bottom of the hole.'
                'The walls rise impossibly high above, the entrance now just a distant pinprick of light.'
            ) asOrderedCollection.
            (self isTorchLit) "Zakładamy, że 'self' tutaj odnosi się do obiektu Game"
                ifTrue: [ descriptionParts add: 'Your torch struggles against the oppressive darkness of this place.' ]
                ifFalse: [ descriptionParts add: 'The darkness here is absolute, suffocating.' ].
            descriptionParts
        ].
        Smalltalk at: #pit_bottom put: pitBottom.

        alchemyLab := Location new.
        alchemyLab name: 'alchemy_lab'.
        alchemyLab description: [
            #(
                'You enter an ancient alchemical workshop. Heavy stone tables are stained with centuries of spilled reagents.'
                'Cracked retorts, dusty alembics, and rusted instruments lie scattered across the workspace.'
                'Shelves line the walls, bearing clay jars with faded labels and the desiccated remains of strange herbs.'
                'In the center of the room, a curious apparatus of brass tubes and glass bulbs stands cold and silent.'
                'Among the debris on the main table, you notice four small vials of different colors: blue, green, purple, and red.'
            )
        ].
        Smalltalk at: #alchemy_lab put: alchemyLab.

        fork1 := Location new.
        fork1 name: 'fork1'.
        fork1 description: [
            #(
                'You stand at a grim intersection where multiple pathways meet.'
                'The air is stagnant here, heavy with the scent of mold and decay.'
                'Carved into the weathered stone walls are strange symbols - warnings, perhaps,'
                'or invocations to forgotten deities that once held sway in these depths.'
                'To the west, an unfamiliar odor wafts from the darkness.'
                'Eastward, you hear distant, rhythmic chanting that raises the hair on your neck.'
                'Northward, the passage descends deeper, and a trail of dark, congealed blood disappears into the shadows.'
                'The floor is marked with countless scratches, as if something was dragged through here repeatedly.'
            )
        ].
        Smalltalk at: #fork1 put: fork1.

        kitchen := Location new.
        kitchen name: 'kitchen'.
        kitchen description: [
            #(
                'You enter what once served as a kitchen for the inhabitants of these catacombs.'
                'A massive stone hearth dominates one wall, its interior blackened with ancient soot.'
                'Long wooden tables, warped and split with age, line the center of the room.'
                'Rusted implements hang from hooks on the walls, and cracked ceramic vessels'
                'lie scattered across shelves and in corners. The air carries a sickly-sweet smell'
                'of spoiled food mingled with something less identifiable. Dark stains cover'
                'the large butchering block in the corner. Something scurries away as you enter.'
            )
        ].
        Smalltalk at: #kitchen put: kitchen.

        chapel := Location new.
        chapel name: 'chapel'.
        chapel description: [
            | descriptionParts |
            descriptionParts := #(
                'You enter a small chapel carved into the living rock. The air is thick with incense'
                'and the sickly-sweet smell of decay. Rows of broken pews face a blackened stone altar,'
                'upon which rest strange artifacts including a small stone pyramid. Dozens of candles cast'
                'dancing shadows across wall carvings depicting scenes of sacrifice and transformation.'
            ) asOrderedCollection.
            (self isPriestAlive) "Zakładamy, że 'self' tutaj odnosi się do obiektu Game"
                ifTrue: [
                    descriptionParts addAll: #(
                        'At the altar stands a figure in tattered robes, facing away from you, muttering'
                        'in a language you do not understand. At the sound of your entrance, the figure'
                        'turns slowly, revealing a gaunt face with sunken eyes that gleam with madness.'
                        'The priest''s lips curl into a terrible smile.'
                    ).
                ]
                ifFalse: [
                    descriptionParts addAll: #(
                        'You enter the defiled chapel, now eerily silent except for the dripping of black ichor'
                        'from the altar. The stone pyramid still rests among the ritual items. Candles burn with'
                        'strange blue flames, casting grotesque shadows of the dead priest''s body across the walls.'
                        'The air feels heavier, as if the ritual space has been further corrupted by the violent death.'
                        'A low, almost subsonic humming emanates from somewhere beneath the altar.'
                    ).
                ].
            descriptionParts
        ].
        Smalltalk at: #chapel put: chapel.

        library := Location new.
        library name: 'library'.
        library description: [
            #(
                'You enter a library filled with dust-covered tomes and scrolls.'
                'Towering bookshelves stretch toward the shadowed ceiling, some tilting precariously under the weight of knowledge.'
                'Parchment scrolls and leather-bound volumes lie scattered across tables and the floor.'
                'Many books are bound in materials that don''t appear to be ordinary leather.'
                'Strange symbols have been scratched into the stone floor, forming a pattern that seems'
                'to converge at the center of the room where a reading podium stands.'
                'Some sections of the bookshelves appear burned, and you notice dark stains trailing'
                'from several tomes as if they had bled.'
                'The air feels heavy with forgotten wisdom and forbidden knowledge.'
            )
        ].
        Smalltalk at: #library put: library.

        fork2 := Location new.
        fork2 name: 'fork2'.
        fork2 description: [
            #(
                'You reach another grim intersection, deeper into the catacombs.'
                'The air here is thick with the scent of damp stone and something faintly metallic.'
                'From the western passage, you hear heavy, rhythmic breathing - something large slumbers there.'
                'To the east, faint whimpers and the distant clink of chains echo through the darkness.'
                'The northern corridor slopes gently downward, carrying the scent of ancient death.'
                'The walls here bear marks of violence - deep gouges in the stone and what appear to be'
                'handprints made in dried blood. The trail continues here growing fresher'
                'and more abundant as it disappears down the eastern passage.'
            )
        ].
        Smalltalk at: #fork2 put: fork2.

        guardQuarters := Location new.
        guardQuarters name: 'guard_quarters'.
        guardQuarters description: [
            | descriptionParts |
            descriptionParts := #(
                'You enter what appears to be living quarters for whatever guards this forsaken place.'
                'Several bunks line the walls, most collapsed with age and decay.'
                'A wooden cabinet against one wall holds numerous bottles of spirits, some still sealed.'
            ) asOrderedCollection.
            (self isGuardAlive)
                ifTrue: [
                    descriptionParts addAll: #(
                        'In the center of the room, sprawled across a massive iron-framed bed, lies what first appears'
                        'to be a guard in enormous plate armor. He appears to be sleeping soundly, breathing heavily.'
                    ).
                ]
                ifFalse: [
                    descriptionParts addAll: #(
                        'The guard quarters are now dominated by the fallen form of the mutated guard.'
                        'Its body continues to dissolve where the acid made contact, revealing a disturbing'
                        'amalgamation of metal, flesh, and something else entirely beneath the armor.'
                        'Black ichor pools around the corpse, bubbling occasionally as if still alive.'
                        'The air feels lighter somehow, as if a malevolent presence has been removed.'
                    ).
                ].
            descriptionParts
        ].
        Smalltalk at: #guard_quarters put: guardQuarters.

        jail := Location new.
        jail name: 'jail'.
        jail description: [
            #(
                'You enter a dank prison chamber carved from the living stone.'
                'Three cells line the far wall, their rusted iron bars casting skeletal shadows in your light.'
                'In the first cell, a gaunt figure sits motionless in the corner, eyes reflecting your light'
                'like those of a nocturnal beast. He makes no sound, but never breaks his stare.'
                'The middle cell contains the withered corpse of what might have once been a man,'
                'now little more than leathery skin stretched over a crooked frame.'
                'From the third cell, a trembling voice whispers: "Please... help me... He... This thing...'
                'what he did to the others... oh gods... please..."'
                'The trail of blood leads to a dark passage on the northern wall,'
                'disappearing into shadows so thick they seem to devour your light.'
                'Countless handprints in dried blood mark the stone around the entrance.'
            )
        ].
        Smalltalk at: #jail put: jail.

        tortureChamber := Location new.
        tortureChamber name: 'torture_chamber'.
        tortureChamber description: [
            #(
                'You enter a chamber of horrors. The air reeks of blood, excrement, and fear.'
                'In the center of the room, suspended from the ceiling by rusted chains,'
                'hangs what was once a man. His body is a canvas of methodical mutilation,'
                'skin flayed in precise patterns. Blood still drips slowly from his wounds,'
                'forming a dark pool beneath him. His eyes are open but unseeing - death approaches,'
                'but has not yet granted mercy.'
                'Against the far wall stands a weapons rack containing a corroded sword.'
            )
        ].
        Smalltalk at: #torture_chamber put: tortureChamber.

        crypt := Location new.
        crypt name: 'crypt'.
        crypt description: [
            #(
                'You enter an ancient crypt, the air thick with the stench of death and decay.'
                'Stone sarcophagi line the walls, their carved lids depicting the tortured faces of those within.'
                'Most have been desecrated, broken open by force, with desiccated remains spilling onto the floor.'
                'In the center of the chamber stands a raised altar of black stone, its surface stained with ancient blood.'
                'Upon it rests a perfect cube of obsidian-black stone.'
                'From somewhere within the sealed sarcophagi, you hear faint scratching sounds, as if something is trying to get out.'
                'Whispers fill your mind when you focus on the obsidian cube, promising terrible knowledge and power.'
                'The floor beneath your feet feels unnaturally warm, and the shadows in the corners seem to move of their own accord.'
            )
        ].
        Smalltalk at: #crypt put: crypt.

        innerSanctum := Location new.
        innerSanctum name: 'inner_sanctum'.
        innerSanctum description: [
            #(
                'You carefully navigate the treacherous spiral staircase, your hands pressed against'
                'the damp stone walls for balance. The steps are unnaturally steep and narrow,'
                'forcing you to descend sideways while hunched over.'
                'After what seems like an eternity of careful descent, the staircase opens into a vast chamber.'
                'Your torch illuminates a horrific scene before you. The chamber is vast, its ceiling lost to darkness.'
                'Massive columns rise like petrified trees, carved with scenes of torture and transformation.'
                'The floor is slick with black ichor that pulses with subtle movement.'
                'And there, sprawled before an obsidian altar, lies the broken body of Sir Galahad.'
                'His once-proud armor is shattered, the metal twisted and fused with his flesh in places.'
                'His face, frozen in an expression of absolute horror, bears signs of transformation - '
                'skin stretched too tight over elongated features, eyes sunken yet somehow too large.'
                'Clutched in his deformed hand is the Chalice of Immaculate Tears, its crystalline surface'
                'glimmering with unholy light. The liquid within moves against gravity, occasionally forming'
                'shapes that resemble faces screaming in agony.'
                'As you approach, Sir Galahad''s corpse twitches. His head turns with a sickening crack,'
                'dead eyes fixing on yours. His jaw drops, dislocating with the movement, and'
                'a voice that is not his whispers: "You are too late, squire. The king is already ours.".'
                'Darkness claims your vision as clawed hands grasp you from behind.'
                'Some things are not meant to be found. Some quests are not meant to succeed.'
                'Your story ends here, in the darkness beneath the earth, joining the countless others'
                'who came seeking power, salvation, or knowledge.'
                'THE END'
            )
        ].
        Smalltalk at: #inner_sanctum put: innerSanctum.


        "--- Definicje ścieżek (paths) ---"
        dungeonEntrance addPath: 'n.' to: mainHall.
        mainHall addPath: 's.' to: dungeonEntrance.
        mainHall addPath: 'e.' to: darkRoom1.
        mainHall addPath: 'w.' to: alchemyLab.
        mainHall addPath: 'n.' to: fork1.
        darkRoom1 addPath: 'beneath' to: pitBottom.
        darkRoom1 addPath: 'w.' to: mainHall.
        alchemyLab addPath: 'e.' to: mainHall.
        fork1 addPath: 's.' to: mainHall.
        fork1 addPath: 'w.' to: kitchen.
        fork1 addPath: 'e.' to: chapel.
        fork1 addPath: 'n.' to: fork2.
        kitchen addPath: 'e.' to: fork1.
        chapel addPath: 'w.' to: fork1.
        chapel addPath: 's.' to: library.
        library addPath: 'n.' to: chapel.
        fork2 addPath: 's.' to: fork1.
        fork2 addPath: 'w.' to: guardQuarters.
        fork2 addPath: 'e.' to: jail.
        fork2 addPath: 'n.' to: crypt.
        guardQuarters addPath: 'e.' to: fork2.
        jail addPath: 'w.' to: fork2.
        jail addPath: 'n.' to: tortureChamber.
        tortureChamber addPath: 's.' to: jail.
        crypt addPath: 's.' to: fork2.
        "Ścieżka mainHall -> innerSanctum jest dodawana dynamicznie przez StatueRoom"


        "--- Item Definitions & Placement ---"

        "dungeon_entrance"
        item := TorchItem newNamed: 'torch' described: #(
            'A wooden torch soaked in pitch. It can provide light in the darkness, but will eventually burn out.'
            'Traveling with a lit torch helps ward off the oppressive darkness, keeping your mind more peaceful.'
        ).
        dungeonEntrance addItem: item.

        item := TakeableItem newNamed: 'dungeon_entrance_note' described: #(
            'A bloodstained parchment with hurried, desperate writing. The message reads:'
            '"I have located the Chalice in the deepest chamber of these accursed catacombs. The whispers... they'
            'speak truths too terrible to bear. Abominations stalk these halls - things that were once men.'
            'Three seals guard the inner sanctum. I have located one near the library archives, but the others...'
            'The walls bleed. The shadows move when unwatched. If you read this, flee while your mind remains yours..."'
        ).
        dungeonEntrance addItem: item.

        "main_hall"
        item := Item newNamed: 'statue' described: #( "Scenery, non-takeable"
            'A grotesque effigy carved from obsidian-black stone. The deity it depicts defies comprehension -'
            'part human, yet wrong in ways your mind struggles to process. Multiple limbs emerge at impossible'
            'angles, and its face bears too many eyes, arranged in a pattern that makes your vision swim.'
            'You''ve never encountered anything resembling this entity in any religious text or traveler''s tale.'
            'The base of the statue contains three distinct recesses, shaped to hold three specific objects:'
            '- a cube'
            '- a pyramid'
            '- a sphere'
            'Looking closer, you notice scrape marks on the stone floor around the statue''s base,'
            'suggesting it has been moved recently. The pattern of the marks hints that the statue'
            'might conceal something hidden beneath it.'
        ).
        mainHall addItem: item.

        "dark_room1"
        item := Item newNamed: 'pit' described: #( "Scenery, non-takeable"
            'The pit descends into impenetrable darkness. You cannot see its bottom, even when'
            'you carefully peer over the edge. The fetid smell makes you recoil involuntarily.'
            'When you drop a small stone in, you hear it bouncing against the walls for several'
            'seconds before a distant splash echoes up.'
        ).
        darkRoom1 addItem: item.

        "pit_bottom"
        item := Item newNamed: 'escape_attempt' described: #( "Scenery, non-takeable, special examine effect"
            'You desperately claw at the walls, trying to climb out of this foul pit.'
            'The stones are coated in centuries of filth and slime, offering no purchase.'
            'Each attempt leaves you sliding back down into the fetid muck below.'
            'After several exhausting tries, the terrible realization sinks in - there is no way out.'
            'This place will be your tomb, just as it was for others before you.'
        ).
        "TODO: examine(escape_attempt) decreases sanity by 100. Needs custom handleExamineAttemptBy:"
        pitBottom addItem: item.

        "alchemy_lab"
        item := OpiumPowderItem newNamed: 'opium_powder'
            described: #(
                'A small jar containing a crude brownish powder derived from poppy seeds.'
                'Such substances were used by medieval healers to dull pain, though none called it by this name.'
                'The jar bears symbols suggesting it will calm the mind and ease suffering.'
            )
            sanityEffect: 20
            hungerEffect: 0
            usedWithDescription: #(
                'You consume some of the opium powder. A numbing calm washes over you,'
                'dulling the terrors of the catacombs. Your mind feels more stable.'
                "TODO: Implement hallucination removal."
            ).
        alchemyLab addItem: item.

        item := ConsumableItem newNamed: 'blue_vial'
            described: #(
                'A vial of luminous blue liquid that seems to swirl of its own accord.'
                'It smells faintly of sea salt and something unidentifiable.'
            )
            sanityEffect: 100 "Player's modifySanityBy: should cap at 100"
            hungerEffect: 0
            usedWithDescription: #(
                'You drink the blue liquid. A wave of clarity washes over you as the catacombs'
                'seem less threatening. Your mind feels restored and focused.'
            ).
        alchemyLab addItem: item.

        item := GreenVialItem newNamed: 'green_vial' "Używamy GreenVialItem"
            described: #(
                'A viscous green substance that occasionally bubbles on its own.'
                'It emits fumes that make your eyes water. The glass of the vial appears to be slowly'
                'corroding from the inside.'
            )
            sanityEffect: -999 "Lethal if drunk by player"
            hungerEffect: 0
            usedWithDescription: #( "Używane, gdy gracz wypija fiolkę"
                'You drink the viscous green liquid, which immediately burns your throat.'
                'To your horror, you feel it eating through your insides like acid.'
                'Your body convulses as the corrosive substance dissolves you from within.'
                'Your last sensation is that of your organs liquefying as darkness claims you.'
            ).
        alchemyLab addItem: item.

        item := PurpleVialItem newNamed: 'purple_vial'
            described: #(
                'A thick purple liquid with tiny specks of light floating within it.'
                'The vial is pleasantly warm to the touch.'
            )
            sanityEffect: 0 
            hungerEffect: 0
            usedWithDescription: #(
                'You drink the purple liquid. The world around you fractures into impossible geometries.'
                'Shadows dance at the edge of your vision, and whispers fill your ears.'
                'Reality itself seems unstable. You fear this effect may be long-lasting.'
                "TODO: Implement hallucination state."
            ).
        alchemyLab addItem: item.

        item := ConsumableItem newNamed: 'red_vial'
            described: #(
                'A crimson fluid that looks disturbingly like blood, yet moves more slowly.'
                'The vial pulses gently, almost like a heartbeat.'
            )
            sanityEffect: 0
            hungerEffect: 0
            usedWithDescription: #(
                'You drink the crimson liquid. At first, nothing happens.'
                'Then your hands begin to tingle unpleasantly. Looking down, you watch in horror'
                'as your flesh begins to rot before your eyes. The decay is spreading slowly.'
                'You must find a way to stop it before it consumes you entirely.'
                "TODO: Implement decaying_hands status and timer."
            ).
        alchemyLab addItem: item.

        "fork1"
        item := TorchItem newNamed: 'torch' described: #(
            'A wooden torch soaked in pitch. It can provide light in the darkness, but will eventually burn out.'
            'Traveling with a lit torch helps ward off the oppressive darkness, keeping your mind more peaceful.'
        ).
        fork1 addItem: item.

        "kitchen"
        item := WeaponItem newNamed: 'kitchen_knife'
            described: #(
                'A large knife with a rusty blade and rotting wooden handle. Despite its condition,'
                'it still holds a threatening edge. Flakes of what might be dried blood cling to the blade.'
            )
            withDefaultUseText: #('You test the edge of the knife against your thumb. Still sharp.').
        kitchen addItem: item.

        item := ConsumableItem newNamed: 'rotten_meat'
            described: #(
                'A chunk of unidentifiable meat, green with mold and crawling with maggots.'
                'The stench emanating from it makes your stomach turn. What kind of creature'
                'this once was is impossible to determine.'
            )
            sanityEffect: -10
            hungerEffect: -15
            usedWithDescription: #(
                'You force yourself to eat the putrid flesh. The taste is foul beyond description,'
                'and the texture makes you gag, but it does quiet the pangs of hunger somewhat.'
                'Your mind recoils at what you''ve just consumed.'
            ).
        kitchen addItem: item.

        item := ConsumableItem newNamed: 'fish'
            described: #(
                'A dried fish, preserved with salt. While certainly not fresh,'
                'it appears to be edible and free from obvious spoilage.'
                'The flesh is firm and the smell, while strong, is not putrid.'
                'It could provide sustenance if you''re desperate enough.'
            )
            sanityEffect: 0
            hungerEffect: -20
            usedWithDescription: #(
                'You tear into the dried fish. The taste is intensely salty and'
                'the texture leathery, but it satisfies your hunger somewhat.'
            ).
        kitchen addItem: item.

        item := ConsumableItem newNamed: 'bread'
            described: #(
                'A surprisingly intact loaf of dark bread. Though stale and hard as stone,'
                'it appears free of mold and might still be edible. It would at least'
                'quiet your growing hunger.'
            )
            sanityEffect: 0
            hungerEffect: -25
            usedWithDescription: #(
                'You force down the stale bread. It''s nearly impossible to chew,'
                'but your gnawing hunger subsides somewhat.'
            ).
        kitchen addItem: item.

        item := ConsumableItem newNamed: 'wine_bottle'
            described: #(
                'An ancient bottle of wine, its label long rotted away. The dark liquid inside'
                'sloshes thickly when you move it. The cork is partially decomposed but still'
                'seals the contents. It might provide a temporary respite from the horrors around you.'
            )
            sanityEffect: 15
            hungerEffect: 5
            usedWithDescription: #(
                'You pull the crumbling cork and drink deeply from the bottle.'
                'The wine has turned to vinegar, but the alcohol still burns pleasantly,'
                'temporarily dulling your awareness of the horrors surrounding you.'
            ).
        kitchen addItem: item.

        "chapel"
        item := Item newNamed: 'priest' described: #(
            'The priest''s skeletal form is draped in rotting ceremonial robes adorned with'
            'strange symbols and dried bloodstains. His skin is pale and stretched too tightly'
            'across his skull. His eyes never blink as they watch your every movement.'
            'There is something unnatural about his movements - jerky, yet fluid, as if his body'
            'is not entirely under his own control. His lips constantly move in silent prayer or curse.'
        ).
        chapel addItem: item.
        
        item := PyramidArtifactItem newNamed: 'pyramid_artifact'
            described: #(
                'A small pyramid carved from obsidian-black stone. Its surface is covered with minute'
                'carvings that seem to shift when not viewed directly. It feels unnaturally cold to the touch,'
                'and somehow heavier than its size would suggest. This appears to match one of the recesses'
                'in the statue base you saw in the main hall.'
            )
            type: #pyramid
            examineSanity: 0
            placeSanity: 0
            placeMessages: #(
                'You carefully place the pyramid artifact into the matching recess in the statue base.'
                'It fits perfectly, and you hear a faint click as it locks into place.'
                'The air grows colder, and whispers fill your mind momentarily. The statue''s eyes briefly glow red.'
            ).
        chapel addItem: item.

        "library"
        item := TakeableItem newNamed: 'alchemy_techniques' described: #(
            'A weathered tome with a cracked leather spine. The pages detail ancient alchemical processes.'
            'You find several relevant sections:'
            '- Opium powder: "A salve for the troubled mind. Calms the nerves and dispels phantasms."'
            '- Blue vial: "Essence of clarity. Restores the mind to perfect equilibrium."'
            '- Green vial: "The universal solvent. Dissolves flesh, bone, and even the hardest armor with ease."'
            '- Purple vial: "Opens the third eye to visions beyond mortal comprehension."'
            '- Red vial: "The curse of living decay. Flesh rots while consciousness remains."'
        ).
        library addItem: item.

        item := TakeableItem newNamed: 'merthveer' described: #(
            'A book bound in what appears to be human skin. The title "Merthveer" is branded onto the cover.'
            'The pages contain disturbing illustrations of rituals performed before a many-limbed deity'
            'with multiple eyes arranged in an unnatural pattern.'
            'The text describes an ancient being that exists beyond human comprehension, whose mere'
            'presence corrupts the mind and warps reality.'
            'Several pages are stained with substances you prefer not to identify.'
            'Reading further makes your vision swim and your sanity waver.'
        ).
        "TODO: examine(merthveer) decreases sanity by 10. Needs custom handleExamineAttemptBy:"
        library addItem: item.

        "jail"
        item := Item newNamed: 'silent_figure' described: #(
            'You approach the bars to get a better look at the silent prisoner.'
            'His skin is unnaturally pale, stretched too tightly over sharp bones.'
            'His eyes never blink as they follow your every movement.'
            'His lips are sewn shut with crude black thread, yet you swear'
            'you can hear faint whispers emanating from him.'
            'As you turn to leave, you notice his shadow doesn''t match his body''s position.'
        ).
        "TODO: examine(silent_figure) decreases sanity by 5. Needs custom handleExamineAttemptBy:"
        jail addItem: item.

        item := Item newNamed: 'human_remains' described: #(
            'The corpse has been here for some time, though the dry air has preserved it somewhat.'
            'The body is contorted in an unnatural position, suggesting a violent end.'
            'The skull has been opened and appears hollow, as if its contents were removed.'
            'There are strange symbols carved into the leathery skin across the chest.'
            'Several fingers and toes appear to have been systematically removed, the wounds cauterized.'
            'A small object glints faintly in the ribcage, partially hidden by the withered organs.'
        ).
        jail addItem: item.

        item := Item newNamed: 'horrified_man' described: #(
            'The man cowers in the far corner of his cell as you approach.'
            'His once-fine clothes hang in tatters, and his body bears countless small, precise cuts.'
            'His eyes are wide with terror as he whispers urgently: "They take us one by one...'
            'down that passage... we hear the screams... then nothing. The guard..."'
            'He clutches something small and metallic in his trembling hands, hiding it'
            'whenever he hears a sound from the passage. "I found a beautiful sphere in this place, black as night.'
            'That thing... the guard... took it from me. Said it belonged to his master..."'
        ).
        jail addItem: item.

        "torture_chamber"
        item := Item newNamed: 'dying_prisoner' described: #(
            'You approach the hanging man, whose breaths come in shallow, wet gasps.'
            'His body has been systematically mutilated - skin peeled away in precise patterns'
            'that form symbols similar to those you''ve seen elsewhere in the catacombs.'
            'His jaw has been removed entirely, and his tongue cut out, ensuring his silence.'
            'One eye has been surgically extracted.'
            'A small key on a chain hangs from a nail driven through his ankle.'
        ).
        tortureChamber addItem: item.

        item := WeaponItem newNamed: 'corroded_dagger'
            described: #(
                'A dagger with a blade corroded by time and strange fluids.'
                'Despite its deteriorated condition, it maintains a wicked edge.'
                'Strange symbols are etched into the blade, visible despite the corrosion.'
                'It feels unnaturally cold to the touch, and somehow heavier than it should be.'
            )
            withDefaultUseText: #('You test the edge of the dagger. Despite its corrosion, it remains surprisingly sharp.').
        tortureChamber addItem: item.

        "guard_quarters"
        item := Item newNamed: 'sleeping_guard' described: #(
            'As you approach cautiously, you realize with horror that this is not armor at all.'
            'The metal plates are fused directly to the creature''s flesh - grown from it, part of it.'
            'Its proportions are wrong - limbs too long, joints bending in impossible directions.'
            'Each breath it takes produces a sound like metal grinding against wet stone.'
            'Its helm-like head has no visor, only a solid plate with crude eyeholes that leak a yellow fluid.'
            'One massive gauntleted hand clutches an iron mace even in sleep.'
            'You pray it doesn''t wake.'
        ).
        guardQuarters addItem: item.
        "sphere_artifact jest tworzony dynamicznie przez GreenVialItem po zabiciu strażnika"

        item := ConsumableItem newNamed: 'whisky'
            described: #(
                'A medium size bottle of amber liquid. The label has long since rotted away.'
                'The liquid inside still sloshes invitingly, preserved by the high alcohol content.'
                'Such spirits might numb your mind to the horrors of this place, at least temporarily.'
            )
            sanityEffect: 25
            hungerEffect: 10
            usedWithDescription: #(
                'You take a long swig of the aged whisky. It burns going down,'
                'but spreads a comforting warmth through your body.'
                'The horrors around you seem slightly more distant now.'
            ).
        guardQuarters addItem: item.

        item := ConsumableItem newNamed: 'vodka'
            described: #(
                'A clear liquid in a dusty bottle. Despite its age, it appears unspoiled.'
                'The strong smell of alcohol burns your nostrils when you remove the cork.'
                'Drinking this might provide temporary relief for your fraying nerves.'
            )
            sanityEffect: 25
            hungerEffect: 10
            usedWithDescription: #(
                'You drink the strong, clear spirit. It sears your throat'
                'but brings a numbing calm to your mind.'
                'Your fear recedes temporarily as the alcohol dulls your senses.'
            ).
        guardQuarters addItem: item.

        "crypt"
        item := ArtifactItem newNamed: 'cube_artifact' "Używamy ArtifactItem"
            described: #(
                'A perfect cube of obsidian-black stone. When you touch it, visions of unspeakable'
                'horror flood your mind - people transformed into twisted amalgamations of flesh and metal,'
                'rituals performed on living subjects whose screams echo through eternity.'
                'It feels unnaturally cold to the touch, and somehow heavier than its size would suggest.'
                'This appears to match one of the recesses in the statue base you saw in the main hall.'
            )
            type: #cube
            examineSanity: -5   "Zgodnie z Prologiem: examine_item(cube_artifact) -> decrease_sanity(5)"
            placeSanity: -10  "Zgodnie z Prologiem: use_item(cube_artifact) w main_hall -> decrease_sanity(10)"
            placeMessages: #(
                'You carefully place the cube artifact into the matching recess in the statue base.'
                'As the cube slots into place, the entire chamber resonates with a deep, subsonic tone.'
                'Shadows in the room appear to elongate, stretching toward the statue. You feel an'
                'uncomfortable pressure in your head, as if something is probing your thoughts.'
            ).
        crypt addItem: item.
    ]
]

Game new run.
