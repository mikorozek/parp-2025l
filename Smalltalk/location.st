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

    removeItem: itemName [
        ^ items removeKey: itemName ifAbsent: [ nil ].
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

    getDescriptionForItemNamed: itemName [
        | item |
        item := items at: itemName ifAbsent: [ nil ].
        item ifNotNil: [
            ^ item description
        ]
        ifNil: [
            ^ Array with: ('You dont see any ', itemName asString, ' here to examine.')
        ]
    ]
]

