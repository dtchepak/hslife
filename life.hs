import Data.List
data Point = Point Int Int deriving (Show, Eq)

tick :: [Point] -> [Point]
tick world = [ cell | cell <- cells_involved_in_this_tick, will_live cell world]
    where cells_involved_in_this_tick = world `union` (flatten (map neighbours world))

is_neighbour :: Point -> Point -> Bool
is_neighbour (Point x1 y1) (Point x2 y2)
    | is_same_point     = False
    | dx<=1 && dy<=1    = True
    | otherwise         = False
    where   dx = abs (x1-x2)
            dy = abs (y1-y2)
            is_same_point = x1 == x2 && y1 == y2

count_neighbours :: Point -> [Point] -> Int
count_neighbours _ [] = 0
count_neighbours pt (x:xs) = 
    let single_neighbour = if is_neighbour pt x then 1 else 0
    in  single_neighbour + count_neighbours pt xs

will_live :: Point -> [Point] -> Bool
will_live pt pts
    | (not is_pt_alive) && number_of_neighbours == 3 = True
    | (not is_pt_alive) = False
    | number_of_neighbours < 2 = False
    | number_of_neighbours > 3 = False
    | otherwise = True
    where   number_of_neighbours = count_neighbours pt pts
            is_pt_alive = pt `elem` pts

neighbours :: Point -> [Point]
neighbours (Point x y) = [(Point (x-1) (y-1)), (Point x (y-1)), (Point (x+1) (y-1)),
                          (Point (x-1) y),                  (Point (x+1) y),
                          (Point (x-1) (y+1)), (Point x (y+1)), (Point (x+1) (y+1))]

-- Run game from GHCI

life :: Int -> [Point] -> IO()
life gens world = putStrLn $ show_gens gens world

-- Run game from stdin

main = interact (show_gens 10 . parse_points (Point 0 0))

-- Display

show_gens :: Int -> [Point] -> [Char]
show_gens 0 _ = []
show_gens gen world = show_world world dimensions ++ "\n" ++ show_gens (gen-1) (tick world)
    where dimensions = world_dimensions world

show_world :: [Point] -> (Point, Point) -> [Char]
show_world world ((Point startX startY), (Point endX endY)) =
    lineBreakAt cells_per_row [ show_point (p `elem` world) | p<-all_points ]
    where
        all_points = [(Point x y) | x<-[startX..endX], y<-[startY..endY]]
        cells_per_row = endX - startX + 1

world_dimensions :: [Point] -> (Point, Point)
world_dimensions [] = ((Point 0 0), (Point 0 0))
world_dimensions cells = ((Point min_x min_y), (Point max_x max_y))
    where max_x = maximum [ x | (Point x y) <- cells ]
          min_x = minimum [ x | (Point x y) <- cells ]
          max_y = maximum [ y | (Point x y) <- cells ]
          min_y = minimum [ y | (Point x y) <- cells ]

-- Converting between Points and Chars
parse_points :: Point -> [Char] -> [Point]
parse_points _ [] = []
parse_points (Point x y) (c:cs)
    | c == '*' = (Point x y) : parse_points (Point (x+1) y) cs
    | c == '\n' = parse_points (Point 0 (y+1)) cs
    | otherwise = parse_points (Point (x+1) y) cs

show_point :: Bool -> Char
show_point True = '*'
show_point False = '.'

-- Utils

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ (flatten xs) 

lineBreakAt :: Int -> [Char] -> [Char]
lineBreakAt n [] = []
lineBreakAt n s = take n s ++ "\n" ++ (lineBreakAt n $ drop n s)

-- Tests

assert_equal :: Eq a => Show a => a -> a -> IO()
assert_equal x y
    | x == y = putStrLn "."
    | otherwise = putStrLn ("Expected " ++ show x ++ ", got " ++ show y)


test :: IO ()
test = do  
        assert_equal True (is_neighbour (Point 1 1) (Point 1 2))
        assert_equal False (is_neighbour (Point 1 1) (Point 1 1))
        assert_equal False (is_neighbour (Point 321 1) (Point 1 1))
        assert_equal 0 (count_neighbours (Point 0 0) [(Point 3 4)])
        assert_equal 1 (count_neighbours (Point 0 0) [(Point 1 1)])
        assert_equal False (will_live (Point 0 0) [])
        assert_equal False (will_live (Point 0 0) [(Point 1 1)])
        assert_equal True (will_live (Point 0 0) [(Point 0 0), (Point 1 0), (Point 1 1)])
        assert_equal True (will_live (Point 0 0) [(Point 0 1), (Point 1 0), (Point 1 1)])
        assert_equal False (will_live (Point 0 0) [(Point 1 0), (Point 1 1)])

-- Examples
beacon = [(Point 0 0), (Point 0 1), (Point 1 0), (Point 1 1), (Point 2 2), (Point 2 3), (Point 3 2), (Point 3 3)]
blinker = [(Point 0 0), (Point 0 1), (Point 0 2)]
