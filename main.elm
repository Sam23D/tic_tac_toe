module Main exposing (main)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing( onClick, onMouseEnter )
import Array exposing (toList, fromList, Array, repeat)
import List exposing (map)

main = Html.beginnerProgram 
    { model = model
    , update = update
    , view = view
    }

type Action 
    = None
    | Press Int {-index of pressed cell-} 
    | ShowHistoicalBoard (Array SquareValue)
    | BoardHovered
    | RestartGame

type SquareValue 
    = Nothing
    | Cross
    | Circle

type Players
    = Player1
    | Player2

type alias HistoryFrame =
    {  board : Array SquareValue, currentPlayer : Players }

type alias Model =
    { board : Array SquareValue
    , renderedBoard : Array SquareValue
    , currentPlayer : Players
    , ended : Bool
    , moveHistory : List HistoryFrame
    , winner : Players
    , gameWon : Bool
    }

model: Model
model = initialModel

initialModel: Model
initialModel = 
    { board = emptyBoardState 9
    , renderedBoard = emptyBoardState 9
    , currentPlayer = Player1
    , ended = False
    , moveHistory = []
    , winner = Player1
    , gameWon = False
    }

update: Action -> Model -> Model
update msg model =
    case msg of 
        None ->
            model
        Press index ->
            processPlayerMove model index
        ShowHistoicalBoard board ->
            { model | renderedBoard = board }
        BoardHovered ->
            { model | renderedBoard = model.board }
        RestartGame ->
            initialModel

view: Model -> Html Action
view model = 
    div 
    [ class "main-container" ]
    [ h1 [][ text "Tic Tac Toe" ]
    , button [ onClick RestartGame, class "restart-button" ][ text "Restart Game" ]
    , viewPlayer model
    , div[ class "board-score-container"]
        [ div[ class "board-container"]
            [ board {} model.renderedBoard
            ]
        , moveHistoryView model.moveHistory
        ]
    ]

viewPlayer: Model -> Html Action
viewPlayer model = 
    if model.gameWon then
        h2 [][ text ( (toString model.winner) ++ " wins!!!"  ) ]
    else if model.ended then
        h2 [][ text ( "Game ended in a tie") ]
    else 
        h2 [][ text ("Playing: " ++ (toString model.currentPlayer)) ]

moveHistoryView : List HistoryFrame -> Html Action
moveHistoryView moveHistory =
    div [ class "history" ]
        [ h3[][ text "Move history" ]
        , ol[] (List.map moveHistoryItemView (List.reverse moveHistory))
    ]
    
moveHistoryItemView :  HistoryFrame -> Html Action
moveHistoryItemView historyMove = 
    li[][
        button [ onClick (ShowHistoicalBoard historyMove.board) ][ text ("See " ++ (toString historyMove.currentPlayer) ++ " move" ) ]
    ]

checkConditionsForMove: Model -> Bool
checkConditionsForMove model =
    if model.ended then
        False
    else
        True

processPlayerMove: Model -> Int -> Model
processPlayerMove model index =
    if checkConditionsForMove model then
        case validMove model.board index of
        True ->
            let
                playerValue = getPlayerSymbol model.currentPlayer
                currentPlayer = nextplayerTurn model.currentPlayer
                newBoard = makeMove model.board index playerValue
                (isInWinConf, player) = hasAnyWinConf newBoard model.currentPlayer
                noMovesLeft = not (anyMoveLeft newBoard)
                isGameEnded = (isInWinConf || noMovesLeft )
            in
                {model 
                    | currentPlayer = currentPlayer
                    , board = newBoard
                    , renderedBoard = newBoard
                    , gameWon = isInWinConf
                    , moveHistory = { board = newBoard, currentPlayer = model.currentPlayer } :: model.moveHistory
                    , ended = isGameEnded
                    , winner = player
                    } 
        False ->
            model
    else
        model

validMove : Array SquareValue -> Int -> Bool
validMove boardState index =
    case Array.get index boardState of
        Just Nothing ->
            True 
        _ ->
            False

nextplayerTurn : Players -> Players
nextplayerTurn user =
    case user of
        Player1 -> Player2
        Player2 -> Player1

makeMove : Array SquareValue -> Int -> SquareValue -> Array SquareValue
makeMove boardState index currValue =
    case Array.get index boardState of
        Just Nothing ->
            Array.set index currValue boardState 
        _ ->
            boardState
getPlayerSymbol : Players -> SquareValue
getPlayerSymbol playerNum = 
    case playerNum of
        Player1 ->
            Cross
        Player2 ->
            Circle

board : config -> Array SquareValue -> Html Action
board config boardState =
    div 
        [ class "board", onMouseEnter BoardHovered ]
        (List.map2 boardCell (toList boardState) (List.range 0 9))

boardCell : SquareValue -> Int -> Html Action 
boardCell innerValue index = 
    button 
        [ onClick ( Press index  ), class "cell-button" ]
        [ text ( boxValue innerValue ) ]

emptyBoardState: Int -> Array SquareValue
emptyBoardState size =
    repeat size Nothing

anyMoveLeft: Array SquareValue -> Bool
anyMoveLeft board =
    List.any (\ cell -> cell == Nothing ) (toList board) 

hasAnyWinConf: Array SquareValue -> Players -> (Bool, Players) 
hasAnyWinConf board player =
    let
        horizontalConf = extractBoardWithConf board horizontalWinConfig
        verticalConf = extractBoardWithConf board verticalWinConfig
        diagonalConf = extractBoardWithConf board diagonalWinConfig
        isWinConfToPlayer = (\ conf -> isWinConfToAValue conf (getPlayerSymbol player))
        extractedConfigurations = List.map isWinConfToPlayer [ horizontalConf, verticalConf, diagonalConf ]
    in
        (List.any isTruthy extractedConfigurations, player)

isTruthy: Bool -> Bool
isTruthy val =
    val == True       

isOfValue : SquareValue -> SquareValue -> Bool
isOfValue boardVal toCompare =
    boardVal == toCompare

extractBoardWithConf : Array SquareValue -> List (List Int) -> List (List SquareValue)
extractBoardWithConf board conf =
    let
        boardList = toList board
    in
        List.map (\ index -> boardValuesAtIndexes board index ) conf

boardValuesAtIndexes : Array SquareValue -> List Int -> List SquareValue
boardValuesAtIndexes board indexList =
    List.map (\ index -> getBoardValue board index) indexList

getBoardValue : Array SquareValue -> Int -> SquareValue
getBoardValue board index =
    let
        given = Array.get index board
    in
        case given of
            Just value ->
                value
            _ ->
                Nothing

isWinConfToAValue : List (List SquareValue) -> SquareValue -> Bool
isWinConfToAValue confOfSqrVals givenValue =
    let
        isOfGivenValue = (\ value -> isOfValue value givenValue ) 
        verifiedValues = (List.map (\ row -> List.all isOfGivenValue row ) confOfSqrVals)
    in
        List.any isTruthy verifiedValues
          
horizontalWinConfig : List (List Int)
horizontalWinConfig = 
    [ [0, 1, 2]
    , [3, 4, 5]
    , [6, 7, 8]
    ]

verticalWinConfig : List (List Int)
verticalWinConfig = 
    [ [0, 3, 6]
    , [1, 4, 7]
    , [2, 5, 8]
    ]

diagonalWinConfig : List (List Int)
diagonalWinConfig = 
    [ [0, 4, 8]
    , [2, 4, 6]
    ]

boxValue : SquareValue -> String
boxValue someValue = 
    case someValue of
        Nothing ->
            ""
        Cross ->
            "X"
        Circle ->
            "O"