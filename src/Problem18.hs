-----------------------------------------------------------------------------
--
-- Maximum path sum I
--
-- By starting at the top of the triangle below and moving to adjacent numbers
-- on the row below, the maximum total from top to bottom is 23.

--    3
--   7 4
--  2 4 6
-- 8 5 9 3
--
-- That is, 3 + 7 + 4 + 9 = 23.
--
-- Find the maximum total from top to bottom of the triangle below:
--
-----------------------------------------------------------------------------

module Problem18 (
    problem18
) where

smallTriangle = "\
\    3    \n\
\   7 4   \n\
\  2 4 6  \n\
\ 8 5 9 3 "

largeTriangle = "\
\                              75                             \n\
\                            95  64                           \n\
\                          17  47  82                         \n\
\                        18  35  87  10                       \n\
\                      20  04  82  47  65                     \n\
\                    19  01  23  75  03  34                   \n\
\                  88  02  77  73  07  63  67                 \n\
\                99  65  04  28  06  16  70  92               \n\
\              41  41  26  56  83  40  80  70  33             \n\
\            41  48  72  33  47  32  37  16  94  29           \n\
\          53  71  44  65  25  43  91  52  97  51  14         \n\
\        70  11  33  28  77  73  17  78  39  68  17  57       \n\
\      91  71  52  38  17  14  91  43  58  50  27  29  48     \n\
\    63  66  04  68  89  53  67  30  73  16  69  87  40  31   \n\
\  04  62  98  27  23  09  70  98  73  93  38  53  60  04  23 "


stringToTriangle :: String -> [[Integer]]
stringToTriangle s = map lineToNumbers $ lines s
	where lineToNumbers l = map read $ words l

-- Combines two rows by taking each item from the first row and adding i to
-- the largest of the two values below it in the second row. The two values
-- below will be the the item at the same index and the subsequent index.
-- We can get a list of the subsequent indexes using tail.
combineRows :: [Integer] -> [Integer] -> [Integer]
combineRows xs ys = zipWith3 maxSum xs ys $ tail ys
	where
		maxSum x y z = x + max y z

-- Folding from the right will combine the rows from the base up. This will
-- leave a single valued list with the maximum path.
problem18 = head $ foldr1 combineRows $ stringToTriangle largeTriangle