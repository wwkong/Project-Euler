-- How many Sundays fell on the first of the month during the twentieth 
-- century (1 Jan 1901 to 31 Dec 2000)?

{-
We start with a few preliminary details.
Each year has 365 days save leap years. From 1901 to 2000 we have 25 leap years. 
Since 366 `mod` 7 = 2, then Jan. 1, 1901 is a Tuesday.
-}

-- Set up a dictionary for the number of days in the format ([month #],[# of days]) excluding leap years
nDays = [(1,31),(2,28),(3,31),(4,30),(5,31),(6,30),
		 (7,31),(8,31),(9,30),(10,31),(11,30),(12,31)]

-- Define the Sunday counter for starting at Jan. 1 for 1901 to 2000
numSundays sDay yearStart yearEnd = numSundays' 0 1 0 yearStart where
	numSundays' dayCount month sCount year
		| year > yearEnd = sCount
		| year `mod` 4 == 0 && month == 2 = -- For leap years
			if onSundayL then numSundays' (nDayCount + 1) (month + 1) (sCount + 1) year
			else numSundays' (nDayCount + 1) (month + 1) sCount year
		| month == 12 = -- For December
			if onSunday then numSundays' nDayCount 1 (sCount + 1) (year + 1)
			else numSundays' nDayCount 1 sCount (year + 1)
		| otherwise = -- All other cases
			if onSunday then numSundays' nDayCount (month + 1) (sCount + 1) year
			else numSundays' nDayCount (month + 1) sCount year
		where 
			nDayCount = dayCount + (snd $ nDays !! (month-1)) -- New day count for each recursion
			onSunday = ((sDay + nDayCount) `mod` 7 == 0) -- Boolean value to check if there is a Sunday on the last day of the current month
			onSundayL = ((sDay + nDayCount + 1) `mod` 7 == 0) -- For leap years
			
			
-- Print and write out the answer
main = do
		let ans = numSundays 2 1901 2000
		writeFile "pe19.txt" $ show ans
		print ans