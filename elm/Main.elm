module Main where

--import F.Flex exposing (..)
import Signal
import String exposing (concat)
import Time exposing (Time, every, second)
import Date exposing (year, hour, minute, second, fromTime)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList)

hoursFactor : Int
hoursFactor = 5 

--hoursBottomRowFactor : Int
--hoursBottomRowFactor = 5 

minutesFactor : Int
minutesFactor = 5 

--minutesBottomRowFactor : Int
--minutesBottomRowFactor = 5 

--import Html.Attributes exposing (style)
main : Signal Html
main = Signal.map currentTime (Time.every Time.second) 


topRowHours : Int -> List Bool
topRowHours h = List.take 4 (List.repeat (h // hoursFactor) True ++ List.repeat 4 False)


bottomRowHours : Int -> List Bool
bottomRowHours h = List.take 4 (List.repeat (rem h hoursFactor) True ++ List.repeat 4 False)


topRowMinutes : Int -> List Bool
topRowMinutes m = List.take 11 (List.repeat (m // minutesFactor) True ++ List.repeat 11 False)


bottomRowMinutes : Int -> List Bool
bottomRowMinutes m = List.take 4 (List.repeat (rem m minutesFactor) True ++ List.repeat 4 False)


getColours : List Bool -> List String
getColours ts = List.map (\x -> if x == True then "Red" else "None") ts


getText : List Bool -> String 
getText ts = String.concat ( getColours ts )


isRed : String -> Bool
isRed c = c == "Red"


mkBigNode : String -> Html
mkBigNode color = div [
    classList [
        ("bigBlocks", True)
        , ("hoursRed", isRed color)
        ]
    ] []


mkNode : String -> Html
mkNode color = div [ 
    classList [ 
        ("blocks", True)
        , ("hoursRed", isRed color)
        ] 
    ] []


getColouredNodes : List String -> List Html
getColouredNodes colors = List.map mkNode colors


getBigColouredNodes : List String -> List Html
getBigColouredNodes colors = List.map mkBigNode colors


currentTime : Time -> Html
currentTime t =
  let date' = fromTime t
      hour' = toString (Date.hour date')
      minute' = toString (Date.minute date')
      second' = toString (Date.second date')
  in 
      div [] [ 
          text ("Time: " ++ hour' ++ " : " ++ minute' ++ " : " ++ second' )
          , div [] (getBigColouredNodes ( getColours ( topRowHours (Date.hour date') ) ) )
          , div [] (getBigColouredNodes ( getColours ( bottomRowHours (Date.hour date') ) ) ) 
          , div [] (getColouredNodes ( getColours ( topRowMinutes (Date.minute date') ) ) )
          , div [] (getBigColouredNodes ( getColours ( bottomRowMinutes (Date.minute date') ) ) )                
          --, div [] [  text ( getText ( bottomRowMinutes (Date.minute date') ) ) ] 

          ]


