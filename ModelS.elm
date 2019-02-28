-- Type definitions for model, berry, and other aspects for Snake-io.elm
module ModelS exposing (..)

type Direction =
      Up
    | Down
    | Left
    | Right

-- For determining location of objects on screen
type alias Point = (Float,Float)
pos : Float -> Float -> Point
pos = (,)

-- Main player cotrolled entity
type alias Snake =
    { front : Point
    , back : List Point
    , direction : Direction }

-- Food, Score, and actual Model type
type alias Berry = Maybe Point

type alias Score = Int

type alias Timer = Int

type Model =
    TitleScreen
    | Initializing Difficulty
    | Playing Snake Berry Score Difficulty
    | GameOver Score Difficulty

type Difficulty
  = Easy
  | Normal
  | Hard
