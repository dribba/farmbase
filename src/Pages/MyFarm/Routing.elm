module Pages.MyFarm.Routing exposing (..)

import UrlParser exposing (..)


type MyFarmRoute
    = MyFarmIndex
    | MyFarmAdd
    | MyFarmEdit Int


matchers : Parser (MyFarmRoute -> a) a
matchers =
    oneOf
        [ map MyFarmIndex (s "my-farm")
        , map MyFarmAdd (s "my-farm" </> s "add")
        , map MyFarmEdit (s "my-farm" </> s "edit" </> int)
        ]


index : String
index =
    "#my-farm"


add : String
add =
    "#my-farm/add"


edit : Int -> String
edit id =
    "#my-farm/edit/" ++ toString id
