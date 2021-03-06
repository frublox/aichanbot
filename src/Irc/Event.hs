module Irc.Event
    ( Event(..)
    )
where

data Event
    = EPrivMsg
    | ENotice
    | ENick
    | EJoin
    | EPart
    | EQuit
    | EMode
    | ETopic
    | EInvite
    | EKick
    | EPing
    | EPong
    | ENumeric Int
    | ERawMsg
    deriving (Eq, Show)
