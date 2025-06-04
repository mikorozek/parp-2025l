Object subclass: Item [
    | name description |

    Item class >> newNamed: aNameString described: aDescriptionArray [
        | instance |
        instance := super new.
        instance initializeWithName: aNameString described: aDescriptionArray.
        ^ instance.
    ]

    initializeWithName: aNameString described: aDescriptionArray [
        name := aNameString asSymbol.
        description := aDescriptionArray
    ]

    name [ ^ name ]
    description [ ^ description ]

    handleTakeAttemptBy: aPlayer inGame: aGame [
        aGame printLines: #('You cannot take that with you.').
        ^ false
    ]

    handleUseAttemptBy: aPlayer inGame: aGame [
        aGame printLines: (Array with: ('You use the ', self name asString, ', but nothing seems to happen.')).
    ]

    handleExamineAttemptBy: aPlayer inGame: aGame [
        aGame printLines: self description.
    ]
]


Item subclass: SceneryItem []


Item subclass: TakeableItem [
    isTakeable [ ^ true ]

    handleTakeAttemptBy: aPlayer inGame: aGame [
        | removedItem |
        removedItem := aPlayer currentLocation removeItemNamed: self name.
        aPlayer addItem: self.
        aGame printLines: (Array with: ('You pick up the ', self name asString, '.')).
    ]
]

TakeableItem subclass: ConsumableItem [
    | sanityChange hungerChange useDescription |

    ConsumableItem class >> newNamed: aNameString described: aDescriptionArray sanityEffect: aSanityDelta hungerEffect: aHungerDelta usedWithDescription: aUseDescriptionArray [
        | instance |
        instance := super new.
        instance initializeWithName: aNameString
                 described: aDescriptionArray
                 sanityEffect: aSanityDelta
                 hungerEffect: aHungerDelta
                 usedWithDescription: aUseDescriptionArray.
        ^ instance.
    ]

    initializeWithName: aNameString described: aDescriptionArray sanityEffect: aSanityDelta hungerEffect: aHungerDelta usedWithDescription: aUseDescriptionArray [
        super initializeWithName: aNameString described: aDescriptionArray.
        sanityChange := aSanityDelta.
        hungerChange := aHungerDelta.
        useDescription := aUseDescriptionArray.
    ]

    sanityChange [ ^ sanityChange ]
    hungerChange [ ^ hungerChange ]
    useDescription [ ^ useDescription ]

    handleUseAttemptBy: aPlayer inGame: aGame [
        aPlayer modifySanityBy: sanityChange.
        aPlayer modifyHungerBy: hungerChange.

        aGame printLines: self useDescription.
        aGame printLines: (Array
            with: ('Sanity changed by: ', sanityChange asString, '. Hunger changed by: ', hungerChange asString, '.')
        ).

        aPlayer removeItemNamed: self name.
    ]
]


TakeableItem subclass: WeaponItem [
    | defaultUseDescription |

    WeaponItem class >> newNamed: aNameString described: aDescriptionArray withDefaultUseText: aDefaultUseDescArray [
        | instance |
        instance := super new.
        instance initializeWithName: aNameString
                 described: aDescriptionArray
                 withDefaultUseText: aDefaultUseDescArray.
        ^ instance.
    ]

    initializeWithName: aNameString described: aDescriptionArray withDefaultUseText: aDefaultUseDescArray [
        super initializeWithName: aNameString described: aDescriptionArray.
        defaultUseDescription := aDefaultUseDescArray.
    ]

    defaultUseDescription [
        ^ defaultUseDescription
    ]

    handleUseAttemptBy: aPlayer inGame: aGame [
        | currentLocation deadPriest game |
        Transcript show: '--- Entering WeaponItem handleUseAttemptBy ---'; cr.
        currentLocation := aPlayer currentLocation.

        (currentLocation name = 'chapel' and: [aGame isPriestAlive])
            ifTrue: [
                aGame printLines: #(
                    'You grip your weapon tightly and approach the priest. His eyes widen in realization'
                    'but the smile never leaves his face. You plunge the blade into his chest. Black ichor,'
                    'not blood, spills from the wound as he collapses to the floor.'
                    'His body crumples, but the laughter continues for several seconds after.'
                ).

                currentLocation removeItemNamed: #priest.
                deadPriest := Item newNamed: 'dead_priest' described: #(
                    'The priest''s corpse lies in a pool of black fluid that seems to move slightly of its own accord.'
                    'His face is frozen in that terrible smile, eyes still open and staring at nothing.'
                    'The wound in his chest pulses faintly, as if something inside is still alive.'
                    'Despite being clearly dead, his fingers occasionally twitch, and you swear'
                    'you can still hear faint whispering coming from his motionless lips.'
                ).
                currentLocation addItem: deadPriest.
                game setPriestAsDead.

                aPlayer removeItemNamed: self name.
                ^ self
            ].

        (currentLocation name = 'guard_quarters' and: [aGame isGuardAlive])
            ifTrue: [
                aGame printLines: #(
                    'The creature rises in agony and rage, its faceplate splitting open to reveal'
                    'rows of needle-like teeth. Though wounded and burning, it hefts its massive'
                    'halberd with terrifying speed. The last thing you see is the blade arcing'
                    'toward your neck before darkness claims you.'
                ).
                aGame gameOver.
                ^ self
            ].

        aGame printLines: self defaultUseDescription.
    ]

]

TakeableItem subclass: WeaponItem [
    | defaultUseDescription |

    WeaponItem class >> newNamed: aNameString described: aDescriptionArray withDefaultUseText: aDefaultUseDescArray [
        | instance |
        instance := super new.
        instance initializeWithName: aNameString
                 described: aDescriptionArray
                 withDefaultUseText: aDefaultUseDescArray.
        ^ instance.
    ]

    initializeWithName: aNameString described: aDescriptionArray withDefaultUseText: aDefaultUseDescArray [
        super initializeWithName: aNameString described: aDescriptionArray.
        defaultUseDescription := aDefaultUseDescArray.
    ]

    defaultUseDescription [
        ^ defaultUseDescription
    ]

    handleUseAttemptBy: aPlayer inGame: aGame [
        | currentLocation deadPriest |
        Transcript show: '--- Entering WeaponItem handleUseAttemptBy ---'; cr.
        currentLocation := aPlayer currentLocation.

        (currentLocation name = 'chapel' and: [aGame isPriestAlive])
            ifTrue: [
                aGame printLines: #(
                    'You grip your weapon tightly and approach the priest. His eyes widen in realization'
                    'but the smile never leaves his face. You plunge the blade into his chest. Black ichor,'
                    'not blood, spills from the wound as he collapses to the floor.'
                    'His body crumples, but the laughter continues for several seconds after.'
                ).

                currentLocation removeItemNamed: #priest.
                deadPriest := Item newNamed: 'dead_priest' described: #(
                    'The priest''s corpse lies in a pool of black fluid that seems to move slightly of its own accord.'
                    'His face is frozen in that terrible smile, eyes still open and staring at nothing.'
                    'The wound in his chest pulses faintly, as if something inside is still alive.'
                    'Despite being clearly dead, his fingers occasionally twitch, and you swear'
                    'you can still hear faint whispering coming from his motionless lips.'
                ).
                currentLocation addItem: deadPriest.
                aGame setPriestAsDead.

                aPlayer removeItemNamed: self name.
                ^ self
            ].

        (currentLocation name = 'guard_quarters' and: [aGame isGuardAlive])
            ifTrue: [
                aGame printLines: #(
                    'The creature rises in agony and rage, its faceplate splitting open to reveal'
                    'rows of needle-like teeth. Though wounded and burning, it hefts its massive'
                    'halberd with terrifying speed. The last thing you see is the blade arcing'
                    'toward your neck before darkness claims you.'
                ).
                aGame gameOver.
                ^ self
            ].

        aGame printLines: self defaultUseDescription.
    ]

]


ConsumableItem subclass: GreenVialItem [
    handleUseAttemptBy: aPlayer inGame: aGame [
        | currentLocation guardQuartersLocation sphereArtifact deadGuardItem |
        currentLocation := aPlayer currentLocation.

        (currentLocation name = 'guard_quarters' and: [aGame isGuardAlive])
            ifTrue: [
                aGame printLines: #(
                    'You carefully unstop the vial and throw the viscous green liquid at the sleeping guard.'
                    'The acid splashes across its metal-fused flesh, instantly hissing and bubbling.'
                    'The creature awakens with a horrific shriek as the corrosive substance eats through'
                    'its armored plates, revealing pulsing, inhuman flesh beneath.'
                ).

                (aPlayer hasWeaponItem)
                    ifTrue: [
                        aGame printLines: #(
                            'Acting on instinct, you lunge forward with your weapon, driving it deep into'
                            'the exposed flesh of its throat. Black ichor sprays from the wound as the creature'
                            'thrashes wildly before collapsing in a heap. Its body continues to twitch'
                            'as the acid dissolves more of its form.'
                            'As its body deteriorates, a perfect black sphere rolls from its remains.'
                        ).
                        aGame setGuardAsDead.
                        guardQuartersLocation := Smalltalk at: #guard_quarters.
                        
                        guardQuartersLocation removeItemNamed: #sleeping_guard.

                        deadGuardItem := Item newNamed: 'dead_guard' described: #(
                            'The guard''s corpse lies in a pool of black fluid, its form partially dissolved by acid.'
                            'The once-fearsome creature is now still, a testament to your desperate act.'
                        ).
                        guardQuartersLocation addItem: deadGuardItem.

                        sphereArtifact := ArtifactItem newNamed: 'sphere_artifact'
                            described: #(
                                'A perfect sphere of obsidian-black stone. No tool marks are visible on its flawless surface.'
                                'It feels unnaturally cold to the touch, and somehow heavier than its size would suggest.'
                                'When you look closely, you see pinpricks of light deep within, like distant stars.'
                                'This appears to match one of the recesses in the statue base you saw in the main hall.'
                            )
                            type: #sphere
                            examineSanity: 0
                            placeSanity: 0
                            placeMessages: #(
                                'You carefully place the sphere artifact into the matching recess in the statue base.'
                                'The sphere settles with an ominous hum, and you hear a faint click as it locks into place.'
                                'The ground beneath you trembles slightly. You notice drops of a black, viscous liquid'
                                'beginning to seep from the statue''s mouth.'
                            ).
                        guardQuartersLocation addItem: sphereArtifact.

                        aPlayer removeItemNamed: self name.
                    ]
                    ifFalse: [
                        aGame printLines: #(
                            'The creature rises in agony and rage, its faceplate splitting open to reveal'
                            'rows of needle-like teeth. Though wounded and burning, it hefts its massive'
                            'halberd with terrifying speed. The last thing you see is the blade arcing'
                            'toward your neck before darkness claims you.'
                        ).
                        aGame gameOver.
                        aPlayer removeItemNamed: self name.
                    ]
            ]
            ifFalse: [
                 super handleUseAttemptBy: aPlayer inGame: aGame.
            ]
    ]
]

TakeableItem subclass: ArtifactItem [
    | artifactType sanityChangeOnExamine sanityChangeOnPlace placementMessages |

    ArtifactItem class >> newNamed: aNameString described: anArray type: aSymbolType examineSanity: anExamineSanityChange placeSanity: aPlaceSanityChange placeMessages: someMessages [
        | instance |
        instance := super new.
        instance initializeWithName: aNameString
                 described: anArray
                 type: aSymbolType
                 examineSanity: anExamineSanityChange
                 placeSanity: aPlaceSanityChange
                 placeMessages: someMessages.
        ^ instance.
    ]

    initializeWithName: aNameString described: anArray type: aSymbolType examineSanity: anExamineSanityChange placeSanity: aPlaceSanityChange placeMessages: someMessages [
        super initializeWithName: aNameString described: anArray.
        artifactType := aSymbolType.
        sanityChangeOnExamine := anExamineSanityChange.
        sanityChangeOnPlace := aPlaceSanityChange.
        placementMessages := someMessages.
    ]

    artifactType [ ^ artifactType ]
    placementMessages [ ^ placementMessages ]
    sanityChangeOnPlace [ ^ sanityChangeOnPlace ]

    handleExamineAttemptBy: aPlayer inGame: aGame [
        super handleExamineAttemptBy: aPlayer inGame: aGame.
        (sanityChangeOnExamine ~= 0) ifTrue: [
            aPlayer modifySanityBy: sanityChangeOnExamine.
        ].
    ]

    handleUseAttemptBy: aPlayer inGame: aGame [
        | currentLocation statueRoom |
        currentLocation := aPlayer currentLocation.

        (currentLocation isKindOf: StatueRoom)
            ifTrue: [
                statueRoom := currentLocation.
                statueRoom placeArtifact: self byPlayer: aPlayer inGame: aGame.
            ]
            ifFalse: [
                aGame printLines: #('This artifact seems to match one of the recesses in the statue in the main hall.').
            ]
    ]
]
