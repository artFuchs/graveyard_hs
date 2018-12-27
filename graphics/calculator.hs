-- seguindo tutorial de como criar uma GUI com o Gtk
-- criando uma calculadora
-- tutorial: https://www.stackbuilders.com/tutorials/haskell/gui-application/
-- diferente do tutorial, estou implementando apenas as operações de soma e subtração

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

-- GUI
main :: IO ()
main = do
  st <- newIORef (Value "" Nothing)

  -- init GTK lib
  initGUI

  -- init window
  window <- windowNew
  set window  [ windowTitle         := "Calculator"
              , windowResizable     := True
              , windowDefaultWidth  := 230
              , windowDefaultHeight := 250]
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False

  -- add a display saying hello
  display <- entryNew
  set display [ entryEditable := False
              , entryXalign   := 1
              , entryText     := "Hello"]

  -- add buttons
  grid <- tableNew 5 4 True
  let attach l r t b item = tableAttachDefaults grid item l r t b
      mkBtn = mkButton st display
  attach 0 4 0 1 display
  mkBtn "7" (enterDigit '7') >>= attach 0 1 1 2
  mkBtn "8" (enterDigit '8') >>= attach 1 2 1 2
  mkBtn "9" (enterDigit '9') >>= attach 2 3 1 2
  mkBtn "C" (clearAll) >>= attach 3 4 1 2
  mkBtn "4" (enterDigit '4') >>= attach 0 1 2 3
  mkBtn "5" (enterDigit '5') >>= attach 1 2 2 3
  mkBtn "6" (enterDigit '6') >>= attach 2 3 2 3
  mkBtn "+" (operator Addition) >>= attach 3 4 2 3
  mkBtn "1" (enterDigit '1') >>= attach 0 1 3 4
  mkBtn "2" (enterDigit '2') >>= attach 1 2 3 4
  mkBtn "3" (enterDigit '3') >>= attach 2 3 3 4
  mkBtn "-" (operator Subtraction) >>= attach 3 4 3 4
  mkBtn "0" (enterDigit '0') >>= attach 0 2 4 5
  mkBtn "=" (equals) >>= attach 3 4 4 5
  containerAdd window grid

  -- show things
  widgetShowAll window
  mainGUI

-- | creates a button, binding a function in it's activation so that it mutates
--   the global state
mkButton :: IORef Value -> Entry -> String -> (Value -> Value) -> IO Button
mkButton st display label mutateState = do
  btn <- buttonNewWithLabel label
  btn `on` buttonActivated $ do
    value <- atomicModifyIORef st $ \x -> let r = mutateState x in (r,r)
    updateDisplay display value
  return btn


-- | 'Value' holds textual representation of first argument reversed and
--   'Operation' to apply to it, which see.
-- 'Value' is also the state of the application
data Value = Value String (Maybe Operation)

-- | Operation to apply to first argument and textual representation of second
--   argument reversed (if relevant).
data Operation = Addition    String
            | Subtraction String

-- | Change second argument of 'Operation'
mapOperation :: (String -> String) -> Operation -> Operation
mapOperation f (Addition    x) = Addition    (f x)
mapOperation f (Subtraction x) = Subtraction (f x)

-- | Get second argument from 'Operation'
getSndArg :: Operation -> String
getSndArg (Addition    x) = x
getSndArg (Subtraction x) = x

-- | Render given 'Value'
renderValue :: Value -> String
renderValue (Value x op) =
  let (a, y) =  case op of
                  Nothing -> ("","")
                  Just (Addition    arg) -> ("+", arg)
                  Just (Subtraction arg) -> ("-", arg)
      f "" = ""
      f l = " " ++ l ++ " "
      g "" = "0"
      g xs = reverse xs
  in g x ++ f a ++ (if null y then "" else g y)

-- | show text in the display
updateDisplay :: Entry -> Value -> IO()
updateDisplay display value = set display [entryText := renderValue value]


-- | change state as if specific digit is entered
enterDigit :: Char -> Value -> Value
enterDigit ch (Value x op) =
  case op of
    Nothing -> Value (ch:x) Nothing
    Just a  -> Value x (Just $ mapOperation (ch:) a)

-- | Apply given operator to current state
operator :: (String -> Operation) -> Value -> Value
operator op value =
  let (Value x operation) = equals value
  in Value x $ Just $
    case operation of
      Nothing -> op ""
      Just a -> op (getSndArg a)

-- | reset calculator state to default state
clearAll :: Value -> Value
clearAll = const (Value "" Nothing)

-- | Evaluate current calculator's state putting result in place of first argument
equals :: Value -> Value
equals (Value x operation) =
  case operation of
    Nothing -> Value x Nothing
    Just a ->
      if null (getSndArg a)
        then Value x operation
        else Value result Nothing
          where g :: String -> Int
                g "" = 0
                g xs = read (reverse xs)
                x' = g x
                y' = g (getSndArg a)
                result = reverse . show $
                  case a of
                    Addition  _ -> x' + y'
                    Subtraction _ -> x' - y'
