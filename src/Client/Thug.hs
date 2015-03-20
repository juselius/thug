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

validateJoin :: a -> Fay Bool
validateJoin _ = do
    jForm    <- getElementById "joinForm"
    jEmail   <- getElementById "joinEmail"
    jName    <- getElementById "joinName"
    jPasswd  <- getElementById "joinPasswd"
    jPasswd_ <- getElementById "joinPasswd_"
    errorBox <- getElementById "errorBox"

    pwOk <- validatePasswd jPasswd jPasswd_ errorBox
    userOk <- validateUser jName jEmail errorBox
    if pwOk && userOk
        then return True
        else return False

validatePasswd :: Element -> Element -> Element -> Fay Bool
validatePasswd e0 e1 msgBox = do
    let p0 = elementValue e0
    let p1 = elementValue e1
    sane p0 p1 msgBox
    where
        sane p0 p1 msgBox
            | p0 /= p1 = do
                setInnerHTML msgBox $ "<b>Passwords don't match!</b>"
                return False
            | length p0 < 8 = do
                setInnerHTML msgBox $ "<b>Password is too short!</b>"
                return False
            | otherwise = return True

validateUser ::  Element -> Element -> Element -> Fay Bool
validateUser u e msgBox = do
    let user = elementValue u
    let email = elementValue e
    setInnerHTML msgBox $ "<b>socket!</b>"
    conn <- newWebSocket "ws://localhost:8080"
    return False


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

alert :: String -> Fay ()
alert = ffi "alert(%1)"

