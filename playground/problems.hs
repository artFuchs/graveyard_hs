import Data.List
import Data.Char

resolveRPN :: String -> Float
resolveRPN expr =
    let subExprs = words expr
        result:rest = foldl (\acc e -> evaluateSubExpr acc e) [] subExprs
    in result

evaluateSubExpr :: [Float] -> String -> [Float]
evaluateSubExpr stack e
  | isNumber c = (read e) : stack
  | e == "+" = (second + first) : rest
  | e == "-" = (second - first) : rest
  | e == "*" = (second * first) : rest
  | e == "/" = (second / first) : rest
  | e == "^" = (second ** first) : rest
  where first:second:rest = stack
        c:_  = e
