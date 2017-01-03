import List exposing (map)
import Html exposing (..)
import Html.Attributes exposing (..)


view_module_name name = span [ class "module_name" ]
  [ text (name ++ ".")
  ]

view_function_header function =
  div [] (List.map view_module_name ["Main"] ++ [ text "main" ] ++ [ text <| " : " ++ " [ Html.Html ] " ] )

view_function function = div []
  [ view_function_header function
  , hr [] []
  , div [] [ text "text \"Hello, World!\""]
  ]



css model = """
* {
  font-family : sans
}

.module_name {
  color: #888;
}
"""

view model = div []
  [ Html.node "style" [] [ text (css model) ]
  , div [ class "code" ] [ view_function () ] ]


main = view ()
