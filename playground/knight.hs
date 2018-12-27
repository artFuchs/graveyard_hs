type KnightPos = (Int,Int)
type Turns = Int

-- dado uma posição, retornar todas as posições possiveis para o cavalo
move :: KnightPos -> [KnightPos]
move (x,y) =
  filter (\(x, y) -> x<9 && y<9 && x>0 && y>0) pos
  where pos = [(x',y') | (x',y') <- [(x-2,y-1),(x-2,y+1),(x+2,y-1),(x+2,y+1),(x-1,y+2),(x+1,y+2),(x-1,y-2),(x+1,y-2)]]

-- dado um numero de turnos e uma posição, retornar uma lista contendo os possíveis movimentos até que os turnos acabem
testMoves :: Turns -> KnightPos -> [KnightPos]
testMoves turns position =
  if turns > 0
    then move position >>= testMoves (turns-1)
    else return position

-- dado uma posição de inicio e uma posição de destino, verificar se o cavalo consegue chegar na posição em exatamente n movimentos
canReachIn :: KnightPos -> KnightPos -> Turns -> Bool
canReachIn ini dest turns = dest `elem` testMoves turns ini
