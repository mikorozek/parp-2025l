Object subclass: Player [
    | currentLocation inventory game sanity hunger |

    Player class >> new [
        | g |
        g := super new.
        g init.
        ^ g.
    ]

    init [
        sanity := 100.
        hunger := 0.
        currentLocation := nil.
        inventory := Dictionary new.
    ]

    changeLocation: location [
        currentLocation := location.
    ]

    currentLocation [
        ^ currentLocation
    ]

    walk: direction [
        | newPosition |
        (#('n.' 'e.' 's.' 'w.' 'beneath.') includes: direction) ifTrue: [
            newPosition := currentLocation locationInGivenDirection: direction.
            newPosition ifNotNil: [
                self changeLocation: newPosition.
                game printLines: currentLocation description.
            ] ifNil: [
                game printLines: #('You cannot go that way.' '').
            ].
        ].
        self modifySanityBy: -2.
        self modifyHungerBy: 2.
    ]

    game: aGame [
        game := aGame.
    ]

    addItem: anItem [
        inventory at: anItem name put: anItem.
    ]

    removeItemNamed: anItemName [
        ^ inventory removeKey: anItemName ifAbsent: [ 
        ].
    ]

    itemInInventoryNamed: anItemName [
        ^ inventory at: anItemName ifAbsent: [ nil ].
    ]

    inventoryItems [
        ^ inventory values.
    ]

    showStatus [
        game printLines: #('Inventory:').

        self inventoryItems isEmpty ifTrue: [
            game printLines: #('  Nothing.')
        ] ifFalse: [
            self inventoryItems do: [ :eachItem |
                | itemNameString line |
                itemNameString := eachItem name asString.
                line := '  - ', itemNameString, ' (1)'.
                game printLines: (Array with: line).
            ].
        ].
        game printLines: (Array with: ('Sanity: ', sanity asString, '%')).
        game printLines: (Array with: ('Hunger: ', hunger asString, '%')).
    ]

    modifySanityBy: anAmount [
        sanity := (sanity + anAmount).
        sanity < 0 ifTrue: [ sanity := 0 ].
        sanity > 100 ifTrue: [ sanity := 100 ].
    ]

    modifyHungerBy: anAmount [
        hunger := (hunger + anAmount).
        hunger < 0 ifTrue: [ hunger := 0 ].
        hunger > 100 ifTrue: [ hunger := 100 ].
    ]

    checkPlayerStatus [
        hunger >= 100 ifTrue: [
            game printLines: #(
                'Overwhelming hunger causes you to collapse. Your body is too weak to continue.'
                'GAME OVER'
            ).
            game gameOver.
        ].
        sanity <= 0 ifTrue: [
            game printLines: #(
                'The darkness starts to consume your mind entirely. You collapse to the floor,'
                'clawing at your own skin, desperate to escape the horrors in your mind.'
                'Your journey ends here, lost in madness beneath the earth.'
                'GAME OVER'
            ).
            game gameOver.
        ].
    ]

    hasWeaponItem [
        inventory values do: [ :item |
            (item isKindOf: WeaponItem) ifTrue: [ ^ true ].
        ].
        ^ false
    ]

    getAWeaponItem [
        inventory values do: [ :item |
            (item isKindOf: WeaponItem) ifTrue: [ ^ item ].
        ].
        ^ nil
    ]
]
