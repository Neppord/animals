module AnimalTree exposing (AnimalTree(..), InsertOperation, StatementRecord, replaceFalse, replaceTrue)


type alias StatementRecord =
    { statement : String
    , true : AnimalTree
    , false : AnimalTree
    }


type AnimalTree
    = Animal String
    | Statement StatementRecord


type alias InsertOperation =
    AnimalTree -> AnimalTree


replaceTrue : InsertOperation -> StatementRecord -> InsertOperation
replaceTrue insertOperation record new_true =
    insertOperation <| Statement { record | true = new_true }


replaceFalse : InsertOperation -> StatementRecord -> InsertOperation
replaceFalse insertOperation record new_false =
    insertOperation <| Statement { record | false = new_false }
