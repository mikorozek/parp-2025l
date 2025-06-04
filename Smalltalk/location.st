Object subclass: Location [
    | name routes items description |

    Location class >> new [
        | g |
        g := super new.
        g init.
        ^ g 
    ]

    init [
        name := 'Location'.
        routes := Dictionary new.
        items := Dictionary new.
    ]

    name [
        ^ name
    ]

    name: aName [
        name := aName
    ]

    description [
        ^ description value
    ]

    description: aDescription [
        description := aDescription
    ]

    addPath: direction to: targetLocation [
        routes at: direction put: targetLocation
    ]

    addItem: item [
        items at: item name put: item.
    ]

    removeItemNamed: anItemName [
        ^ items removeKey: anItemName ifAbsent: [ nil ].
    ]

    locationInGivenDirection: direction [
        ^ routes at: direction ifAbsent: [ nil ]
    ]

    availablePathsDescription [
        ^ self buildDescriptionListFor: routes withHeader: 'Available Paths:' andFormatter: [ :assoc |
            '  - ', assoc key, ' (to ', assoc value name, ')'
        ]
    ]

    itemsDescription [
        items isEmpty ifTrue: [
            ^ #('There are no visible items here.')
        ].
        ^ self buildDescriptionListFor: items withHeader: 'You see the following items here:' andFormatter: [ :assoc |
            '  - ', assoc key
        ].
    ]

    buildDescriptionListFor: aDictionary withHeader: aHeaderString andFormatter: aBlock [
        | resultStrings |
        resultStrings := OrderedCollection new.
        resultStrings add: aHeaderString.

        aDictionary associationsDo: [ :assoc |
            resultStrings add: (aBlock value: assoc).
        ].

        ^ resultStrings asArray.
    ]

    getItemNamed: itemName [
        ^ items at: itemName asSymbol ifAbsent: [ nil ].
    ]
]

Location subclass: StatueRoom [
    | artifactsPlacedCount maxArtifacts pathNameToInnerSanctum innerSanctumLocationName innerSanctumTarget |

    StatueRoom class >> new [
        | instance |
        instance := super new.
        instance initializeStatueRoom.
        ^ instance.
    ]

    initializeStatueRoom [
        artifactsPlacedCount := 0.
        maxArtifacts := 3. 
        pathNameToInnerSanctum := 'beneath.'.
        innerSanctumLocationName := #inner_sanctum.
    ]

    maxArtifacts: anInteger [ 
        maxArtifacts := anInteger 
    ]

    pathNameToInnerSanctum: aString [ 
        pathNameToInnerSanctum := aString 
    ]

    innerSanctumLocationName: aSymbol [ 
        innerSanctumLocationName := aSymbol 
    ]

    placeArtifact: anArtifactItem byPlayer: aPlayer inGame: aGame [
        aGame printLines: anArtifactItem placementMessages.

        (anArtifactItem sanityChangeOnPlace ~= 0) ifTrue: [
            aPlayer modifySanityBy: anArtifactItem sanityChangeOnPlace.
        ].
        
        aPlayer removeItemNamed: anArtifactItem name.
        
        artifactsPlacedCount := artifactsPlacedCount + 1.

        self checkAllArtifactsPlacedInGame: aGame.
    ]

    checkAllArtifactsPlacedInGame: aGame [
        (artifactsPlacedCount >= maxArtifacts)
            ifTrue: [
                aGame printLines: #(
                    'As the final artifact is placed, the statue begins to tremble violently.'
                    'The eyes of the deity blaze with unholy light, and the mouth opens wider, disgorging'
                    'a torrent of black ichor that pools around the base.'
                    'With a grinding sound of stone against stone, the floor beneath the statue'
                    'splits open, revealing an impossibly narrow, spiral staircase carved into the bedrock.'
                    'The steps are worn and treacherously steep, descending at an unnatural angle into absolute darkness.'
                    'A cold draft rises from below, carrying whispers in a language you cannot comprehend.'
                    'The descent appears hazardous, but now accessible.'
                ).

                innerSanctumTarget := Smalltalk at: innerSanctumLocationName.

                innerSanctumTarget ifNotNil: [
                    self addPath: pathNameToInnerSanctum to: innerSanctumTarget.
                ] ifNil: [ Transcript show: 'DEBUG StatueRoom: innerSanctumTarget was nil. Path NOT added.'; cr.
                ].
            ]
    ]
]
