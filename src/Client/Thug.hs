{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE EmptyDataDecls  #-}
{-# LANGUAGE EmptyDataDecls  #-}

module Thug (
    validateJoin
  ) where

import Prelude hiding (log)
import FFI

data Element
data WebSocket
data Event

class Eventable a
instance Eventable WebSocket
instance Eventable Element

-- main :: Fay ()
-- main = do
--     validateJoin

alert :: String -> Fay ()
alert = ffi "alert(%1)"

validateJoin :: a -> Fay Bool
validateJoin _ = do
    joinForm    <- getElementById "joinForm"
    joinEmail   <- getElementById "joinEmail"
    joinName    <- getElementById "joinName"
    joinPasswd  <- getElementById "joinPasswd"
    joinPasswd_ <- getElementById "joinPasswd_"

    alert("hej")
    return False

    -- if elementValue joinPasswd /= elementValue joinPasswd_
    --     then do
    --         setInnerHTML joinPasswd_ "Passwords don't match"
    --         return False
    --     else
    --         return True


    -- prompt <- getElementById "prompt"
    -- messages <- getElementById "repl"
    -- connectionToSanta <- newWebSocket "ws://localhost:8080"

    -- addEventListener connectionToSanta "onopen" $ \_ -> do
    --     setDisabled prompt False
    --     addEventListener prompt "onkeydown" $ \e -> do
    --         if (eventKeyCode e == 13)
    --         then do
    --             let message = elementValue prompt
    --             connectionToSanta `send` message
    --             log messages message "me"
    --             clearValue prompt
    --         else return ()

    -- addEventListener connectionToSanta "onmessage" $ \e -> do
    --     log messages (messageData e) ""

-- log :: Element -> String -> String -> Fay ()
-- log container msg cls = do
--     logNode <- createElement "li"
--     setInnerHTML logNode msg
--     setClassName logNode cls
--     container `appendChild` logNode

createElement :: String -> Fay Element
createElement = ffi "document.createElement(%1)"

setInnerHTML :: Element -> String -> Fay ()
setInnerHTML = ffi "%1['innerHTML'] = %2"

setClassName :: Element -> String -> Fay ()
setClassName = ffi "%1['innerHTMl'] = %2"

appendChild :: Element -> Element -> Fay ()
appendChild = ffi "%1.appendChild(%2)"

getElementById :: String -> Fay Element
getElementById = ffi "document.getElementById(%1)"

newWebSocket :: String -> Fay WebSocket
newWebSocket = ffi "new WebSocket(%1)"

addEventListener :: Eventable a => a -> String -> (Event -> Fay ()) -> Fay ()
addEventListener = ffi "%1[%2] = %3"

setDisabled  :: Element -> Bool -> Fay ()
setDisabled = ffi "%1.disabled = %2"

eventKeyCode :: Event -> Int
eventKeyCode = ffi "%1.keyCode"

clearValue :: Element -> Fay ()
clearValue = ffi "%1.value = ''"

send :: WebSocket -> String -> Fay ()
send = ffi "%1.send(%2)"

elementValue :: Element -> String
elementValue = ffi "%1.value"

messageData :: Event -> String
messageData = ffi "%1.data"

