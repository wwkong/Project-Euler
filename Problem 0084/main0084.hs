{-

In the game, Monopoly, the standard board is set up in the following way:

GO  A1  CC1 A2  T1  R1  B1  CH1 B2  B3  JAIL
H2                                      C1
T2                                      U1
H1                                      C2
CH3                                     C3
R4                                      R2
G3                                      D1
CC3                                     CC2
G2                                      D2
G1                                      D3
G2J F3  U2  F2  F1  R3  E3  E2  CH2 E1  FP

A player starts on the GO square and adds the scores on two 6-sided dice to determine the number of squares they advance in a clockwise direction. Without any further rules we would expect to visit each square with equal probability: 2.5%. However, landing on G2J (Go To Jail), CC (community chest), and CH (chance) changes this distribution.

In addition to G2J, and one card from each of CC and CH, that orders the player to go directly to jail, if a player rolls three consecutive doubles, they do not advance the result of their 3rd roll. Instead they proceed directly to jail.

At the beginning of the game, the CC and CH cards are shuffled. When a player lands on CC or CH they take a card from the top of the respective pile and, after following the instructions, it is returned to the bottom of the pile. There are sixteen cards in each pile, but for the purpose of this problem we are only concerned with cards that order a movement; any instruction not concerned with movement will be ignored and the player will remain on the CC/CH square.

Community Chest (2/16 cards):
1. Advance to GO
2. Go to JAIL

Chance (10/16 cards):
1. Advance to GO
2. Go to JAIL
3. Go to C1
4. Go to E3
5. Go to H2
6. Go to R1
7. Go to next R (railway company)
8. Go to next R
9. Go to next U (utility company)
10. Go back 3 squares.

The heart of this problem concerns the likelihood of visiting a particular square. That is, the probability of finishing at that square after a roll. For this reason it should be clear that, with the exception of G2J for which the probability of finishing on it is zero, the CH squares will have the lowest probabilities, as 5/8 request a movement to another square, and it is the final square that the player finishes at on each roll that we are interested in. We shall make no distinction between "Just Visiting" and being sent to JAIL, and we shall also ignore the rule about requiring a double to "get out of jail", assuming that they pay to get out on their next turn.

By starting at GO and numbering the squares sequentially from 00 to 39 we can concatenate these two-digit numbers to produce strings that correspond with sets of squares.

Statistically it can be shown that the three most popular squares, in order, are JAIL (6.24%) = Square 10, E3 (3.18%) = Square 24, and GO (3.09%) = Square 00. So these three most popular squares can be listed with the six-digit modal string: 102400.

If, instead of using two 6-sided dice, two 4-sided dice are used, find the six-digit modal string.own.

-- Dictionary of positions
posns :: [(Int, String)]
posns = [(0,"GO"),(1,"A1"),(2,"CC1"),(3,"A2"),(4,"T1"),(5,"R1"),(6,"B1"),(7,"CH1"),(8,"B2"),
         (9,"B3"),(10,"JAIL"),(11,"C1"),(12,"U1"),(13,"C2"),(14,"C3"),(15,"R2"),(16,"D1"),
         (17,"CC2"), (18,"D2"),(19,"D3"),(20,"FP"),(21,"E1"),(22,"CH2"),(23,"E2"),(24,"E3"),
         (25,"R3"),(26,"F1"),(27,"F2"),(28,"U2"),(29,"F3"),(30,"G2J"),(31,"G1"),(32,"G2"),
         (33,"CC3"),(34,"G3"),(35,"R4"),(36,"CH3"),(37,"H1"),(38,"T2"),(39,"H2")]

-}

-- import           Debug.Trace
import           Data.List                     (group, sort)
import           Data.Word                     (Word64)
import qualified System.Random.Mersenne.Pure64 as M

-- Set up high level data types
data Position = Position {nDoubles :: Int, index :: Int} deriving (Show, Eq)

-- Roll a four sided dice with a generator
rollDice4 :: M.PureMT -> (Int, M.PureMT)
rollDice4 gen = ((abs k `mod` 4) + 1, g)
    where
        (k, g) = M.randomInt gen

-- Roll a six sided dice with a generator
rollDice6 :: M.PureMT -> (Int, M.PureMT)
rollDice6 gen = ((abs k `mod` 6) + 1, g)
    where
        (k, g) = M.randomInt gen

-- Find the next position if we hit a community chest square
moveCommunityChest :: (Int, (M.PureMT, Int)) -> (Int, (M.PureMT, Int))
moveCommunityChest (doubles, (gen, posn))
    -- | trace ("card = " ++ show card) False = undefined
    | card == 1 = (doubles, (genNext, 0))
    | card == 2 = (0,       (genNext, 10))
    | otherwise = (doubles, (genNext, posn))
    where
        (k, genNext) = M.randomInt gen
        card = (abs k `mod` 16) + 1

-- Find the next position if we hit a chance square
moveChance :: (Int, (M.PureMT, Int)) -> (Int, (M.PureMT, Int))
moveChance (doubles, (gen, posn))
    -- | trace ("card = " ++ show card) False = undefined
    | card == 1  = (doubles, (genNext, 0))
    | card == 2  = (0,       (genNext, 10))
    | card == 3  = (doubles, (genNext, 11))
    | card == 4  = (doubles, (genNext, 24))
    | card == 5  = (doubles, (genNext, 39))
    | card == 6  = (doubles, (genNext, 5))
    | card == 7  = (doubles, (genNext, (nextR  + 40) `mod` 40))
    | card == 8  = (doubles, (genNext, (nextR  + 40) `mod` 40))
    | card == 9  = (doubles, (genNext, (nextU  + 40) `mod` 40))
    | card == 10 = (doubles, (genNext, (posn-3 + 40) `mod` 40))
    | otherwise =  (doubles, (genNext,  posn))
    where
        (k, genNext) = M.randomInt gen
        card = (abs k `mod` 16) + 1
        nextR = snd . minimum $ filter (\(a,_) -> a>0) [(rs-posn, rs) | rs <- [5,15,25,35,45]]
        nextU = snd . minimum $ filter (\(a,_) -> a>0) [(us-posn, us) | us <- [12,28,     52]]

-- Find the next position for a given pair of Seed Generator and Position for six-sided die
movePosition :: ((M.PureMT, Position), M.PureMT -> (Int, M.PureMT)) ->
                ((M.PureMT, Position), M.PureMT -> (Int, M.PureMT))
movePosition ((gen, posn), dRoller)
    -- | trace ("d1 = " ++ show d1 ++ ", d2=" ++ show d2) False = undefined
    | nextDoubles == 3  = ((genLast, Position {nDoubles = 0,            index = 10}      ), dRoller) -- Triple double
    | nextPosn    == 30 = ((genLast, Position {nDoubles = 0,            index = 10}      ), dRoller) -- Go to Jail
    | otherwise         = ((genLast, Position {nDoubles = nextDoubles,  index = nextPosn}), dRoller) -- Other
    where
        (d1, genNext1) = dRoller gen
        (d2, genNext2) = dRoller genNext1
        curDoubles = nDoubles posn
        rolledDoubles = if d1 == d2 then curDoubles + 1 else 0
        (nextDoubles, (genLast, nextPosn))
            | landedPosn `elem` [7,22,36] = moveChance         (rolledDoubles,  (genNext2, landedPosn)) -- Chance
            | landedPosn `elem` [2,17,33] = moveCommunityChest (rolledDoubles,  (genNext2, landedPosn)) -- Community Chest
            | otherwise                   = (rolledDoubles, (genNext2, landedPosn))                     -- Other
            where
                landedPosn = (d1 + d2 + index posn) `mod` 40


-- Run a simulated game with k rolls of a given starting seed and dice roller
runGame :: Int -> Word64 -> (M.PureMT -> (Int, M.PureMT)) -> [Position]
runGame k seed dRoller = take k $ map (snd . fst) $ iterate movePosition ((startGen, startPosn), dRoller)
    where
        -- Initialize
        startGen  = M.pureMT seed
        startPosn = Position {nDoubles = 0, index = 0}

-- Given a list of positions, find the three most frequent squares and print the 6-digit modal string
top3 :: [Position] -> [(Int, Int)]
top3 posns = take 5 digits
    where
        digitGroups  = group . sort $ map index posns
        digits       = map (\(a,b)->(-a,b)) . sort $ map (\a -> (-length a, head a)) digitGroups

-- Print and write
main :: IO()
main = do
        let game6 = runGame 100000 23434235 rollDice6 -- Pick a random seed
        let game4 = runGame 100000 14423423 rollDice4 -- Pick a random seed
        let test = top3 game6
        let ans  = top3 game4
        writeFile "pe84.txt" $ show ans
        print $ "6-sided Die: " ++ show test
        print $ "4-sided Die: " ++ show ans
        -- mapM_ print game6
