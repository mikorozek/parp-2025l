Object subclass: Item [
    | name description |

    Item class >> newNamed: aNameString described: descriptionArray [
        | instance |
        instance := super new.
        instance initializeWithName: aNameString described: descriptionArray.
        ^ instance.
    ]

    initializeWithName: aNameString described: descriptionArray [
        name := aNameString asSymbol.
        description := descriptionArray
    ]

    name [ ^ name ]
    description [ ^ description ]
]
