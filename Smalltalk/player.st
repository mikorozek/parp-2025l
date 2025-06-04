Object subclass: Player [
    | currentLocation inventory sanity hunger isHallucinating hallucinationMessageIndex isTorchLit torchTurnsRemaining |

    Player class >> new [
        | g |
        g := super new.
        g init.
        ^ g 
    ]

    init [
        sanity := 100.
        hunger := 0.
        currentLocation := nil.
        inventory := Dictionary new.
        isHallucinating := false.
        hallucinationMessageIndex := 0.
        isTorchLit := false.
        torchTurnsRemaining := 0.
    ]

    isHallucinating [ ^ isHallucinating ]
    isHallucinating: aBoolean [ isHallucinating := aBoolean ]

    isTorchLit [ ^ isTorchLit ]
    isTorchLit: aBoolean [ isTorchLit := aBoolean ]

    torchTurnsRemaining [ ^ torchTurnsRemaining ]
    torchTurnsRemaining: anInteger [ torchTurnsRemaining := anInteger ]

    lightNewTorch [
        self isTorchLit: true.
        self torchTurnsRemaining: 20.
    ]

    updateTorchStatusInGame: aGame [
        (self isTorchLit) ifTrue: [
            self torchTurnsRemaining: (self torchTurnsRemaining - 1).
            (self torchTurnsRemaining > 0)
                ifTrue: [
                    aGame printLines: (Array with: ('Your torch burns dimmer. ', self torchTurnsRemaining asString, ' turns remaining.'))
                ]
                ifFalse: [
                    self isTorchLit: false.
                    aGame printLines: #('Your torch flickers and dies, plunging you into darkness.')
                ]
        ].
    ]

    changeLocation: aLocation [
        currentLocation := aLocation.
    ]
    
    currentLocation [
        ^ currentLocation
    ]

    walk: direction inGame: aGame [
        | newPosition |
        (#('n.' 'e.' 's.' 'w.' 'beneath.') includes: direction) ifTrue: [
            newPosition := currentLocation locationInGivenDirection: direction.
            newPosition ifNotNil: [
                self changeLocation: newPosition.
                aGame printLines: currentLocation description.
                (currentLocation name = 'inner_sanctum') ifTrue: [
                    aGame gameOver.
                ].
            ] ifNil: [
                aGame printLines: #('You cannot go that way.' '').
            ].
        ].
        self endTurnMaintenanceInGame: aGame.
    ]

    triggerNextHallucinationFor: aGame [
        | messages selectedIndex |
        messages := aGame hallucinationMessages.
        messages isEmpty ifTrue: [ ^ self ].

        hallucinationMessageIndex := hallucinationMessageIndex + 1.
        (hallucinationMessageIndex > messages size) ifTrue: [
            hallucinationMessageIndex := 1.
        ].
        selectedIndex := hallucinationMessageIndex.
        aGame printLines: (Array with: (messages at: selectedIndex)).
    ]

    endTurnMaintenanceInGame: aGame [
        | sanityDrainToApply |

        self updateTorchStatusInGame: aGame.

        self isHallucinating
            ifTrue: [
                sanityDrainToApply := -4.
                self triggerNextHallucinationFor: aGame.
            ]
            ifFalse: [
                sanityDrainToApply := -3.
                (sanity <= 30 and: [sanity > 15]) ifTrue: [
                    self triggerNextHallucinationFor: aGame.
                ].
                (sanity <= 15 and: [sanity > 0]) ifTrue: [
                    self triggerNextHallucinationFor: aGame.
                ].
            ].

        (self isTorchLit) ifFalse: [
            sanityDrainToApply := sanityDrainToApply * 2.
        ].

        self modifySanityBy: sanityDrainToApply.
        self modifyHungerBy: 3.
        self checkGameOverConditionsInGame: aGame.
    ]

    checkGameOverConditionsInGame: aGame [
        hunger >= 100 ifTrue: [
            aGame printLines: #(
                'Overwhelming hunger causes you to collapse. Your body is too weak to continue.'
            ).
            aGame gameOver.
        ].
        sanity <= 0 ifTrue: [
            aGame printLines: #(
                'The darkness starts to consume your mind entirely. You collapse to the floor,'
                'clawing at your own skin, desperate to escape the horrors in your mind.'
                'Your journey ends here, lost in madness beneath the earth.'
            ).
            aGame gameOver.
        ].
    ]

    addItem: anItem [
        inventory at: anItem name put: anItem.
    ]

    removeItemNamed: anItemName [
        ^ inventory removeKey: anItemName ifAbsent: [ 
            nil
        ].
    ]

    itemInInventoryNamed: anItemName [
        ^ inventory at: anItemName ifAbsent: [ nil ].
    ]

    inventoryItems [
        ^ inventory values.
    ]

    showStatusInGame: aGame [
        aGame printLines: #('Inventory:').

        self inventoryItems isEmpty ifTrue: [
            aGame printLines: #('  Nothing.')
        ] ifFalse: [
            self inventoryItems do: [ :eachItem |
                | itemNameString line |
                itemNameString := eachItem name asString.
                line := '  - ', itemNameString, ' (1)'.
                aGame printLines: (Array with: line).
            ].
        ].
        aGame printLines: (Array with: ('Sanity: ', sanity asString, '%')).
        aGame printLines: (Array with: ('Hunger: ', hunger asString, '%')).
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
