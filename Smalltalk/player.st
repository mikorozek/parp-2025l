Object subclass: Player [
    | currentLocation inventory game |

    Player class >> new [
        | g |
        g := super new.
        g init.
        ^ g.
    ]

    init [
        currentLocation := nil.
        inventory := Array new.
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
    ]

    game: aGame [
        game := aGame.
    ]

    addItem: newItem [
        inventory := inventory copyWith: newItem.
    ]

    removeItem: item [
        inventory remove: item.
    ]

    hasItem: item [
        ^ inventory includes: item.
    ]

    showPosition [
        currentLocation printNl.
    ]

    showInventory [
        Transcript show: 'Inventory: '.
        inventory do: [:item |
            Transcript show: item named, ' '.
        ].
        Transcript cr.
    ]
]
