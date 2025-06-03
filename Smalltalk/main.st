"The germ of a text adventure game
Marcin Szlenk 2024"

"Game class

Responsibilities:
* print/read outputs/inputs
* handle game loop
* handle 'instructions' and 'halt' commands

Collaborators:
* ..."

Object subclass: Game [
    | isGameOver player isTorchLit isGuardAlive isPriestAlive |


    introductionText := #(
        'The year is 1433. A bitter wind carries the scent of decay as darkness falls over the land.'
        'Sir Galahad, once revered as the most valiant of the king''s knights, was dispatched to these'
        'accursed catacombs with a desperate mission - to retrieve the Chalice of Immaculate Tears,'
        'the only hope for our monarch who writhes in agony as black veins spread beneath his skin.'
        'Three moons have waxed and waned since Sir Galahad departed into these forsaken lands.'
        'As his loyal squire, you now stand where he once stood, the weight of duty like a millstone.'
        'The locals speak only in whispers of what dwells below - abominations born of forbidden alchemy,'
        'tortured souls that wander the eternal darkness, and horrors that feast upon sanity itself.'
        'Hunger and fear will be your constant companions in this labyrinth of the damned.'
    ).


    instructionsText := #(
        'Available commands are:'
        ''
        'instructions  -- to see these instructions.'
        'paths         -- to see available paths from your current location.'
        'items         -- to see available items in your current location.'
        'quit          -- to end the game and quit.'
        'n.  s.  e.  w.  beneath.     -- to go in that direction.'
        ''
    ).
 
    Game class >> new [
        | g |
        g := super new.
        g init.
        ^g
    ]

    isTorchLit [
        ^ isTorchLit
    ]

    isGuardAlive [
        ^ isGuardAlive
    ]

    isPriestAlive [
        ^ isPriestAlive
    ]

    init [
        isGameOver := false.
        isTorchLit := true.
        isGuardAlive := true.
        isPriestAlive := true.
        player := Player new.
        player game: self.
        self initializeWorld.
        player changeLocation: (Smalltalk at: #dungeon_entrance).
    ]

    "Print strings from array in separate lines."
    printLines: anArray [
        anArray do: [ :string | Transcript show: string; cr ]
    ]

    printIntroduction [
       self printLines: introductionText.
    ]

    printInstructions [
        self printLines: instructionsText.
    ]

    readCommand [
        Transcript show: '> '.
        ^ FileStream stdin nextLine
    ]

    "Run the game."
    run [
        | cmd cmdParts isUnknown itemNameString |

        self printIntroduction.
        self printInstructions.

        "Game loop."
        [isGameOver] whileFalse: [

            isUnknown := true.
            cmd := self readCommand.
            cmdParts := cmd substrings.

            (#('n.' 's.' 'e.' 'w.' 'beneath.') includes: cmd) ifTrue: [
                player walk: cmd.
                isUnknown := false.
            ].

            cmd = 'paths' ifTrue: [
                self printLines: (player currentLocation availablePathsDescription).
                isUnknown := false.
            ].

            cmd = 'items' ifTrue: [
                self printLines: (player currentLocation itemsDescription).
                isUnknown := false.
            ].

            cmd = 'instructions' ifTrue: [
                self printInstructions.
                isUnknown := false.
            ].

            cmd = 'quit' ifTrue: [
                isGameOver := true.
                isUnknown := false.
            ].

            (cmdParts size = 2 and: [cmdParts first = 'examine']) ifTrue: [
                itemNameString := cmdParts second.
                self printLines: (player currentLocation getDescriptionForItemNamed: (itemNameString asSymbol)).
                isUnknown := false.
            ].
                
            isUnknown ifTrue: [
                self printLines: #('Unknown command.' '').

            ]
        ]
    ]
]
