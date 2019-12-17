module Event (
    Event(..)
)
where 
data Event = START | MOVE_UP | MOVE_LEFT | MOVE_DOWN | MOVE_RIGHT | EXIT deriving (Show, Eq)