{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE EmptyDataDecls  #-}

module Thug (
      validateJoin
    ) where

import Prelude hiding (log)
import FFI

data Element
data WebSocket
data Event

data User = User {
      firstName :: String
    , lastName :: String
    , email :: String
    , passwd :: String
    } deriving (Show)

class Eventable a
instance Eventable WebSocket
instance Eventable Element

validateJoin :: a -> Fay Bool
validateJoin _ = do
    jForm       <- getElementById "joinForm"
    errorBox    <- getElementById "errorBox"

    jEmail      <- getElementById "joinEmail"
    jFirstName  <- getElementById "joinFirstName"
    jLastName   <- getElementById "joinLastName"
    jPasswd     <- getElementById "joinPasswd"
    jPasswd_    <- getElementById "joinPasswd_"

    let usr = User {
          firstName = elementValue jFirstName
        , lastName  = elementValue jLastName
        , email     = elementValue jEmail
        , passwd    = elementValue jPasswd
        }

    pwOk <- validatePasswd jPasswd jPasswd_ errorBox
    registerUser usr errorBox
    s <- waitReply
    let userOk = False
    s <- getElementById "errorBox"
    alert $ s
    if pwOk && userOk
        then return True
        else return False

waitReply :: Fay String
waitReply = do
    st <- getElementById "joinStatus"
    if null (elementValue st)
        then waitReply
        else return (elementValue st)

validatePasswd :: Element -> Element -> Element -> Fay Bool
validatePasswd e0 e1 msgBox = do
    let p0 = elementValue e0
    let p1 = elementValue e1
    sane p0 p1 msgBox
    where
        sane p0 p1 msgBox
            | p0 /= p1 = do
                setInnerHTML msgBox "<b>Passwords don't match!</b>"
                return False
            | length p0 < 8 = do
                setInnerHTML msgBox "<b>Password is too short!</b>"
                return False
            | otherwise = return True

registerUser :: User -> Element -> Fay ()
registerUser u msgBox = do
    conn <- newWebSocket "ws://localhost:8080"
    addEventListener conn "onopen" $ \_ -> do
        conn `send` "new thug"
        conn `send` show u
    addEventListener conn "onmessage" $ \e -> do
        let m = messageData e
        setInnerHTML msgBox m
        st <- getElementById "errorBox"
        setInnerHTML st "foop"

createElement :: String -> Fay Element
createElement = ffi "document.createElement(%1)"

setInnerHTML :: Element -> String -> Fay ()
setInnerHTML = ffi "%1['innerHTML'] = %2"

setClassName :: Element -> String -> Fay ()
setClassName = ffi "%1['innerHTMl'] = %2"

getElementById :: String -> Fay Element
getElementById = ffi "document.getElementById(%1)"

getInnerHTML :: Element -> String
getInnerHTML = ffi "%1.innerHTML"

elementValue :: Element -> String
elementValue = ffi "%1.value"

addEventListener :: Eventable a => a -> String -> (Event -> Fay ()) -> Fay ()
addEventListener = ffi "%1[%2] = %3"

addEventListener' :: Eventable a => a -> String -> (Event -> Fay Bool) -> Fay Bool
addEventListener' = ffi "%1[%2] = %3"

appendChild :: Element -> Element -> Fay ()
appendChild = ffi "%1.appendChild(%2)"

setDisabled  :: Element -> Bool -> Fay ()
setDisabled = ffi "%1.disabled = %2"

eventKeyCode :: Event -> Int
eventKeyCode = ffi "%1.keyCode"

clearValue :: Element -> Fay ()
clearValue = ffi "%1.value = ''"

newWebSocket :: String -> Fay WebSocket
newWebSocket = ffi "new WebSocket(%1)"

send :: WebSocket -> String -> Fay ()
send = ffi "%1.send(%2)"

messageData :: Event -> String
messageData = ffi "%1.data"

alert :: String -> Fay ()
alert = ffi "alert(%1)"

