{-
In the card game poker, a hand consists of five cards and are ranked, from lowest to highest,
in the following way:

High Card: Highest value card.
One Pair: Two cards of the same value.
Two Pairs: Two different pairs.
Three of a Kind: Three cards of the same value.
Straight: All cards are consecutive values.
Flush: All cards of the same suit.
Full House: Three of a kind and a pair.
Four of a Kind: Four cards of the same value.
Straight Flush: All cards are consecutive values of same suit.
Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.

The cards are valued in the order:
2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

If two players have the same ranked hands then the rank made up of the highest value wins; for example,
a pair of eights beats a pair of fives (see example 1 below). But if two ranks tie, for example, both
players have a pair of queens, then highest cards in each hand are compared;
if the highest cards tie then the next highest cards are compared, and so on.

The file, poker.txt, contains one-thousand random hands dealt to two players.
Each line of the file contains ten cards (separated by a single space): the first
five are Player 1's cards and the last five are Player 2's cards.

You can assume that all hands are valid (no invalid characters or repeated cards),
each player's hand is in no specific order, and in each hand there is a clear winner.

How many hands does Player 1 win?
-}

import           Data.List
import           Data.Maybe
import qualified Data.Set   as S

-- Initialize some useful helper functions

-- Returns the value ranks in order
handValRanks :: [(Char,Char)] -> [Int]
handValRanks hand = sort $ map (valRank . fst) hand

-- Takes a hand, applies the above, and returns the appropriate ordering when dealing with a 
-- hand rank deadlock
handValDeadlock :: [(Char,Char)] -> [Int]
handValDeadlock hand =
    map snd $ sortBy (\(a,_) (c,_) -> (compare c a)) $ sortBy (\(_,b) (_,d) -> (compare d b))
        [(length val, head val) | val <- vals]
    where vals = group $ handValRanks hand

-- Checks if a hand contains cards all of the same suit
sameSuit :: [(Char,Char)] -> Bool
sameSuit hand = (length . nub . (map snd)) hand == 1

-- Checks if a hand has consecutive elements
consecElems :: [(Char,Char)] -> Bool
consecElems hand = (map (\n -> (n - minV)) values) == [0,1,2,3,4]
    where
        values = sort $ handValRanks hand
        minV = head values

-- Returs the groups of a hand
pokerGroups :: [(Char,Char)] -> [Int]
pokerGroups hand =  sort $ ((map length) . group . sort) (map fst hand)

-- Create a dictionary of Poker rankings
valRank :: Char -> Int
valRank val =   fromJust $ lookup val
                [('2',2),('3',3),('4',4),('5',5),('6',6),('7',7),('8',8),
                ('9',9),('T',10),('J',11),('Q',12),('K',13),('A',14)]

handRank :: [(Char,Char)] -> Int
handRank hand
    | sameSuit hand &&
    (S.fromList ['T','J','Q','K','A']) `S.isSubsetOf` handVals = 10 -- Royal Flush
    | sameSuit hand && consecElems hand                        = 9 -- Straight Flush
    | 4 `elem` (pokerGroups hand)                              = 8 -- Four of a Kind
    | pokerGroups hand == [2,3]                                = 7 -- Full House
    | sameSuit hand                                            = 6 -- Flush
    | consecElems hand                                         = 5 -- Straight
    | 3 `elem` (pokerGroups hand)                              = 4 -- Three of a kind
    | pokerGroups hand == [1,2,2]                              = 3 -- Two pair
    | 2 `elem` (pokerGroups hand)                              = 2 -- One pair
    | otherwise                                                = 1 -- High card
    where handVals = S.fromList $ map fst hand

-- Define a function that takes two poker hands and returns 1 if the first hand wins and 2 otherwise
winningHand :: [(Char,Char)] -> [(Char,Char)] -> Int
winningHand player1 player2
    | (handRank player1) > (handRank player2) = 1
    | (handRank player1) < (handRank player2) = 2
    | (handValDeadlock player1) > (handValDeadlock player2) = 1
    | (handValDeadlock player1) < (handValDeadlock player2) = 2

-- Now create a function to parse each line in the text file
parsePokerLine :: [[Char]] -> ([(Char,Char)],[(Char,Char)])
parsePokerLine = (splitAt 5) . (map (\(n:s) -> (n, head s)))

-- Define the input parse function
parse :: String -> [([(Char,Char)],[(Char,Char)])]
parse = (map parsePokerLine) . (map words) . lines

-- Print and write out the answer
main :: IO()
main = do
        pokerHands <- fmap parse $ readFile "poker.txt"
        let ans = length [games | games <- pokerHands,  winningHand (fst games) (snd games) == 1]
        writeFile "pe54.txt" $ show ans
        print ans