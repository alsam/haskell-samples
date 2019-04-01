-- -*- Haskell -*-

----------------------------------------------------------------------
--  Created:            Saturday, June 17, 2006
--  Original author:    Alexander Samoilov
----------------------------------------------------------------------

--
--  Brief file description here
--

-- generates text files in .opb format for routing benchmarks

module Main
  where

import IO
import Char
import System
import CPUTime
import List

-- type and data definitions

type Point = (Int,Int)

type Rect = (Point,Point)

type Net = [Point]

data TestCase = TestCase {

  -- grid size X * Y
  grid      :: (Int,Int),

  -- net terminals : list length is equal to # of net, each terminal is a (X,Y) pair
  terminals :: [Net],

  -- keepout regions
  keepouts  :: [Rect],

  -- for 2 pin net routing in a single layer
  check_intersect :: Bool

}


test_case :: [TestCase]

test_case = [

  -- !! 1
  (TestCase (5,7)
           [[(4,2),(2,7)], [(5,2),(1,7)]]
           [((1,5),(4,6))]
           True -- in 1 layer
  ),

  -- !! 2
  (TestCase (5,10) --(6,12) --(5,10)
           --[[(5,2), (1,5), (5,6), (1,7), (5,8), (4,10)]] -- one 6-terminal net
           --[[(1,2), (1,6), (1,8), (2,10), (5,5), (5,7)]] -- one 6-terminal net
--           [[(1,3), (5,2), (2,3), (3,4), (3,6), (1,5), (5,6), (1,7), (5,8), (4,10), (2,10)]] -- one 6-terminal net
           [[(1,1), (5,2), (2,3), (3,4), (3,6), (1,5), (5,5), (1,7), (5,8), (4,10), (2,10)]] -- one 6-terminal net
           [((4,4),(5,8))]                                            -- keepout
           False                                         -- route in 2 layers
  ),

  -- !! 3
  (TestCase (17,24)
           [[(10,1),(1,19),(17,19),(17,22)], [(17,4),(17,20),(8,1),(4,24)]]
           [] -- no keepouts at all
           False -- in 2 layers
  ),

-- unstisfiable
-- test_case4 =
--  TestCase (5,10)
--           [ [(1,1), (5,2), (2,3), (3,4), (3,6), (1,5), (5,5), (1,7), (5,8), (4,10), (2,10)],
--             [(1,3), (3,1), (5,9)]
--           ]
--           [((4,4),(5,8))]
--           False -- in 2 layers

  -- !! 4
  (TestCase (5,10)
           [ [(1,1), (5,2), (2,3), (1,5), (5,5), (1,7), (5,8), (4,10), (2,10)],
             [(1,3), (3,1), (5,9), (1,6), (1,8), (1,10), (3,10)]
           ]
           [((4,4),(5,8))]
           False -- in 2 layers
  ),

  -- !! 5 TODO modify me
  (TestCase (10,15)
           [ [(1,1), (10,2), (2,3), (1,5), (10,5), (1,7), (10,8), (4,15), (2,15)],
             [(1,3), (3,1), (10,9), (1,6), (1,8), (1,15), (3,15)],
             [(10,15), (6,1), (9,1), (1,11), (10,12)]
           ]
           [((4,4),(5,8)), ((5,6),(7,7)), ((5,11),(6,14)), ((4,12),(7,13))]
           False -- in 2 layers
  ),

  -- !! 6 TODO modify me
  (TestCase (5,10)
           [ [(1,1), (5,2), (2,3), (1,5), (5,5), (1,7), (5,8), (4,10), (2,10)],
             [(1,3), (3,1), (5,9), (1,6), (1,8), (1,10), (3,10)]
           ]
           [((4,4),(5,8))]
           False -- in 2 layers
  ),

  -- !! 7  -- 24 nets on a grid 17 X 24
  (TestCase (17,24)
           [ [(10,1),(1,19),(17,19),(17,22)],                 -- 1
             [(17,4),(17,20),(8,1),(4,24)],                   -- 2
             [(2,1),(15,24)],                                 -- 3
             [(4,1),(1,5),(17,5)],                            -- 4
             [(17,10),(1,8)],                                 -- 5
             [(1,7),(17,8)],                                  -- 6
             [(17,7),(1,6)],                                  -- 7
             [(1,10),(17,11)],                                -- 8
             [(1,9),(1,12),(17,9),(9,1)],                     -- 9
             [(3,1),(1,17)],                                  -- 10
             [(12,1),(8,24)],                                 -- 11
             [(17,6),(6,1),(1,13)],                           -- 12
             [(17,12),(13,1)],                                -- 13
             [(17,14),(14,1)],                                -- 14
             [(17,2),(17,13),(17,15),(1,14),(1,16),(16,24)],  -- 15
             [(5,1),(1,4)],                                   -- 16
             [(7,1),(1,3)],                                   -- 17
             [(17,23),(1,23),(10,24),(6,24),(2,24)],          -- 18
             [(17,21),(14,24)],                               -- 19
             [(17,18),(9,24),(11,24)],                        -- 20
             [(17,17),(7,24)],                                -- 21
             [(1,22),(3,24)],                                 -- 22
             [(1,18),(5,24)],                                 -- 23
             [(11,1),(1,2),(1,15),(13,24)]                    -- 24

           ]
           [] -- no keepouts at all
           False -- in 2 layers
  )
  ]


gen :: Handle -> TestCase -> Bool -> IO ()
gen h tc print_ast =

  let grid_size = grid tc
      nets  = terminals tc
      num_nets = length nets
  in

  do

    putStrLn "-I- generating equations for objective functions"
    hPutStrLn h "* equations for objective functions"
    gen_min_objective h num_nets grid_size

    putStrLn "-I- generating equations for exterior grid borders"
    hPutStrLn h "* equations for exterior grid borders"
    gen_exterior_borders h num_nets grid_size

    let kors = keepouts tc
    if (length kors >= 1)
      then
        do
          putStrLn "-I- generating equations for keepout regions"
          hPutStrLn h "*equations for keepout regions"
          mapM_ (\kor -> gen_ko_region h num_nets kor grid_size) kors
      else
        putStr ""

    putStrLn "-I- generating equations for congestion constraints"
    hPutStrLn h "* equations for congestion constraints"
    gen_congestion_constraints h num_nets grid_size

    if (not $ check_intersect tc)
      then
        do
          putStrLn "-I- generating additional equations for exterior grid borders\n-I- for multi-terminal nets"
          hPutStrLn h "* additional equations for exterior grid borders for multi-terminal nets"
          gen_exterior_borders_multy h nets grid_size

          putStrLn "-I- generating additional equations for congestion constraints\n-I- for multi-terminal nets"
          hPutStrLn h "* additional equations for congestion constraints for multi-terminal nets"
          gen_congestion_constraints_multy h nets grid_size

          putStrLn "-I- generating additional equations for vias constraints\n-I- for multi-layer nets"
          hPutStrLn h "* additional equations for vias constraints for multi-layer nets"
          gen_vias_constraints h num_nets grid_size

          --------------

          putStrLn "-I- generating equations for intersect constraints"
          hPutStrLn h "* equations for intersect constraints"
          gen_intersect_constraints_multy h nets grid_size -----

      else
          putStr ""

    putStrLn "-I- generating equations for connectivity constraints"
    gen_connectivity_constraints h nets grid_size

    if (check_intersect tc)
      then
        do
          putStrLn "-I- generating equations for intersect constraints"
          hPutStrLn h "* equations for intersect constraints"
          gen_intersect_constraints h nets grid_size
      else
        putStr ""

  where

    -- applies map on a list [a] after that flats result via foldr(1) akin FL's flatmap
    flatmap :: (a -> [b]) -> [a] -> [b]
    flatmap f list = foldr (++) [] (map f list)

    show_term :: Int -> String -> [Int] -> String
    show_term multiplier term multi_index =
      let ast_or_blank = if print_ast then "*" else " " in
      let (i:j:net_number:xs) = multi_index in
      " " ++ sign ++ (show multiplier) ++ ast_or_blank ++ term ++ (show net_number) ++ (flatmap (\x -> "_" ++ (show x)) xs)
          ++ "_" ++ (show i) ++ "_" ++ (show j)
        where sign = if multiplier >= 0 then "+" else ""

    gen_min_objective :: Handle -> Int -> (Int,Int) -> IO ()
    gen_min_objective h num_nets (m,n) =
      do
        putStrLn $ "print_ast: " ++ (show print_ast)
        hPutStrLn h $ "min: " ++ objective ++ " ;\n"
      where
        indices = [[i,j,k] | i<-[0..m], j<-[0..n], k<-[1..num_nets]]
        pr var_name = show_term 1 var_name
        objective = flatmap (\index -> (pr "h" index) ++ (pr "v" index)) indices
    
    gen_ko_region :: Handle -> Int -> Rect -> (Int,Int) -> IO ()
    gen_ko_region h num_nets rect (m,n) =
      hPutStrLn h $ constraint ++ left_constraint ++ down_constraint
      where
        (ll,ru) = rect
        (lly,llx) = ll
        (ruy,rux) = ru
        indices = [[i,j,k] | i<-[lly..ruy-1], j<-[llx..rux-1], k<-[1..num_nets]]
        left_indices = [[i,j,k] | i<-[lly..ruy-1], j<-[llx-1], k<-[1..num_nets]]
        down_indices = [[i,j,k] | i<-[lly-1], j<-[llx..rux-1], k<-[1..num_nets]]
        pr var_name index = (show_term 1 var_name index) ++ " = 0 ;\n"
        constraint = flatmap (\index -> (pr "h" index) ++ (pr "v" index)) indices
        left_constraint = flatmap (\index -> pr "h" index) left_indices
        down_constraint = flatmap (\index -> pr "v" index) down_indices
    
    gen_exterior_borders :: Handle -> Int -> (Int,Int) -> IO ()
    gen_exterior_borders h num_nets (m,n) =
      hPutStrLn h $ left_constraint ++ "\n" ++ down_constraint ++ "\n"
                 ++ right_constraint ++ "\n" ++ up_constraint ++ "\n"
      where
        -- counter clockwise, beginning from west
        left_border  = [[i,j,k] | i<-[1..m], j<-[0], k<-[1..num_nets]]
        down_border  = [[i,j,k] | i<-[0], j<-[1..n], k<-[1..num_nets]]
        right_border = [[i,j,k] | i<-[1..m], j<-[n], k<-[1..num_nets]]
        up_border    = [[i,j,k] | i<-[m], j<-[1..n], k<-[1..num_nets]]
        pr var_name index = (show_term 1 var_name index) ++ " = 0 ;\n"
        constraint var_name border = flatmap (\index -> pr var_name index) border
        left_constraint  = constraint "h" left_border
        down_constraint  = constraint "v" down_border
        right_constraint = constraint "h" right_border
        up_constraint    = constraint "v" up_border
    
    gen_congestion_constraints :: Handle -> Int -> (Int,Int) -> IO ()
    gen_congestion_constraints h num_nets (m,n) =
      hPutStrLn h constraint
      where
        edge_cap = 1 -- TODO consider moving it to function parameter
        pr var_name index = show_term 1 var_name index
        var_constr var_name i j =
          flatmap (\net_ind -> pr var_name [i,j,net_ind]) [1..num_nets] ++ " <= " ++ (show edge_cap) ++ " ;\n"
        full_constr (i,j) = (var_constr "v" i j) ++ (var_constr "h" i j)
        constraint = flatmap full_constr [(i,j) | i<-[1..m], j<-[1..n]]
    
    gen_connectivity_constraints :: Handle -> [Net] -> (Int,Int) -> IO ()
    gen_connectivity_constraints h nets (m,n) =
      hPutStrLn h equations
      where
        num_nets = length nets
        indiced_nets = zip [1..num_nets] nets
        simple_nets = filter (\(index,net) -> (length net) <= 2) indiced_nets
        indices = [(i,j,knet) | i<-[1..m], j<-[1..n], knet<-simple_nets]
        constraint :: (Int,Int,(Int,Net)) -> String
        constraint (i,j,(k,net)) =
          let is_term = any (\(i',j') -> (i' == i) && (j' == j)) in
          connectivity_stencil (is_term net) (i,j) [k]
    
        equations = flatmap (\ind -> constraint ind ) indices
    
    gen_intersect_constraints :: Handle -> [Net] -> (Int,Int) -> IO ()
    gen_intersect_constraints h nets (m,n) =
      hPutStrLn h equations
      where
        num_nets = length nets
        all_terms = flatmap id nets
        indices = [(i,j) | i<-[1..m], j<-[1..n]]
        pr var_name index = show_term 1 var_name index
    
        cell_constr (i,j) =
          let is_term = any (\(i',j') -> (i' == i) && (j' == j)) in
          (flatmap
           (\k -> (pr "v" [i-1, j, k]) ++ (pr "h" [i, j-1, k]) ++ (pr "v" [i, j, k]) ++ (pr "h" [i, j, k]))
           [1..num_nets])
              ++ (if (is_term all_terms) then " <= 1;\n" else " <= 2;\n")
    
        equations = flatmap (\ind -> cell_constr ind) indices
    
    gen_intersect_constraints_multy :: Handle -> [Net] -> (Int,Int) -> IO ()
    gen_intersect_constraints_multy h nets (m,n) =
      hPutStrLn h equations
      where
        num_nets = length nets
        all_terms = flatmap id nets
        indices = [(i,j) | i<-[1..m], j<-[1..n]]
        pr var_name index = show_term 1 var_name index
    
        cell_constr_v (i,j) =
          let is_term = any (\(i',j') -> (i' == i) && (j' == j))
          in (flatmap
              (\k -> (pr "v" [i-1, j, k]) ++ (pr "v" [i, j, k]))
              [1..num_nets])
              ++ (if (is_term all_terms) then " <= 1;\n" else " <= 2;\n")
    
        cell_constr_h (i,j) =
          let is_term = any (\(i',j') -> (i' == i) && (j' == j))
          in (flatmap
              (\k -> (pr "h" [i, j-1, k]) ++ (pr "h" [i, j, k]))
              [1..num_nets])
              ++ (if (is_term all_terms) then " <= 1;\n" else " <= 2;\n")
    
        equations = flatmap (\ind -> (cell_constr_v ind) ++ (cell_constr_h ind)) indices
    
    gen_vias_constraints :: Handle -> Int -> (Int,Int) -> IO ()
    gen_vias_constraints h num_nets (m,n) =
      do
        hPutStrLn h equations1
        hPutStrLn h equations2
      where
        indices = [(i,j) | i<-[1..m], j<-[1..n]]
        pr var_name index = show_term 1 var_name index
    
        cell_constr1 (i,j) =
          let net_inds = [k | k<-[1..num_nets]]
          in (flatmap
              (\k -> pr "c" [i, j, k])
              net_inds) ++ " <= 1;\n"
    
        cell_constr2 (i,j,k) =
          (show_term (-1) "h" $ [i, j,   k]) ++ (show_term (-1) "v" $ [i,   j, k]) ++ (show_term 1 "c" $ [i, j, k]) ++ " >= -1;\n" ++
          (show_term (-1) "h" $ [i, j,   k]) ++ (show_term (-1) "v" $ [i-1, j, k]) ++ (show_term 1 "c" $ [i, j, k]) ++ " >= -1;\n" ++
          (show_term (-1) "h" $ [i, j-1, k]) ++ (show_term (-1) "v" $ [i,   j, k]) ++ (show_term 1 "c" $ [i, j, k]) ++ " >= -1;\n" ++
          (show_term (-1) "h" $ [i, j-1, k]) ++ (show_term (-1) "v" $ [i-1, j, k]) ++ (show_term 1 "c" $ [i, j, k]) ++ " >= -1;\n"
    
        equations1 = flatmap (\ind -> cell_constr1 ind) indices
        equations2 = flatmap (\ind -> cell_constr2 ind) [(i,j,k) | i<-[1..m], j<-[1..n], k<-[1..num_nets]]

    -- more complicated stuff:
    -- 1. multi-terminal nets

    split_net :: Net -> [Net]
    split_net net =
      let (driver:sinks) = net
      in [[d,s] | d<-[driver], s<-sinks]

    gen_exterior_borders_multy :: Handle -> [Net] -> (Int,Int) -> IO ()
    gen_exterior_borders_multy h nets (m,n) =
      hPutStrLn h $ left_constraint ++ "\n" ++ down_constraint ++ "\n"
                 ++ right_constraint ++ "\n" ++ up_constraint ++ "\n"
      where
        -- counter clockwise, beginning from west
        num_nets = length nets
        indiced_nets = zip [1..num_nets] nets
        multi_nets = filter (\(index,net) -> (length net) > 2) indiced_nets
        subnet_indices = map (\(k,net) -> (k, length $ split_net net)) multi_nets
        left_border  = [[i,j,k,l] | i<-[1..m], j<-[0], x<-subnet_indices, k<-[fst x], l<-[1..(snd x)]]
        down_border  = [[i,j,k,l] | i<-[0], j<-[1..n], x<-subnet_indices, k<-[fst x], l<-[1..(snd x)]]
        right_border = [[i,j,k,l] | i<-[1..m], j<-[n], x<-subnet_indices, k<-[fst x], l<-[1..(snd x)]]
        up_border    = [[i,j,k,l] | i<-[m], j<-[1..n], x<-subnet_indices, k<-[fst x], l<-[1..(snd x)]]
        pr var_name index = (show_term 1 var_name index) ++ " = 0 ;\n"
        constraint var_name border = flatmap (\index -> pr var_name index) border
        left_constraint  = constraint "h" left_border
        down_constraint  = constraint "v" down_border
        right_constraint = constraint "h" right_border
        up_constraint    = constraint "v" up_border

    gen_congestion_constraints_multy :: Handle -> [Net] -> (Int,Int) -> IO ()
    gen_congestion_constraints_multy h nets (m,n) =
      hPutStrLn h constraint
      where
        edge_cap = 1 -- TODO consider moving it to function parameter
        num_nets = length nets
        indiced_nets = zip [1..num_nets] nets
        multi_nets = filter (\(index,net) -> (length net) > 2) indiced_nets

        dep_constr :: String -> Int -> Int -> (Int,Net) -> String
        dep_constr var_name i j (k,multi_net) =
          let mnet = split_net multi_net in
          let n_subnets = length mnet in
          let subvars = flatmap (\l -> show_term 1 var_name [i,j,k,l]) [1..n_subnets] in
          (show_term (-1)         var_name [i,j,k]) ++ subvars ++ " >= 0;\n" ++
          (show_term (-n_subnets) var_name [i,j,k]) ++ subvars ++ " <= 0;\n"

        subvar_constr :: Int -> Int -> (Int,Net) -> String
        subvar_constr  i j (k,multi_net) =
          let mnet = split_net multi_net in
          let n_subnets = length mnet in
          let indiced_subnets = zip [1..n_subnets] mnet in
          flatmap (\(l,subnet) ->
            --connectivity_stencil (any (\(i',j') -> (i' == i) && (j' == j)) subnet) (i,j) [k,l]) indiced_subnets
            connectivity_stencil_vias (any (\(i',j') -> (i' == i) && (j' == j)) subnet) (i,j) [k,l]) indiced_subnets

        full_constr (i,j,kn) = (dep_constr "v" i j kn) ++ (dep_constr "h" i j kn) ++ (subvar_constr i j kn)
        constraint = flatmap full_constr [(i,j,kn) | i<-[1..m], j<-[1..n], kn<-multi_nets]

    -- main connectivity equations w/o vias
    connectivity_stencil ::  Bool -> Point -> [Int] -> String
    connectivity_stencil is_term (i,j) free_indices =
      if is_term
      then -- terminal cell
        (show_term 1 "h" $ [i,   j-1] ++ free_indices) ++ (show_term 1 "h" $ [i, j] ++ free_indices) ++ 
        (show_term 1 "v" $ [i-1, j]   ++ free_indices) ++ (show_term 1 "v" $ [i, j] ++ free_indices) ++ " = +1;\n"
      else -- pass cell
    
        -- a = v_{i-1,j} b = h_{i,j-1} c = v_{i,j} d = h_{i,j}
        -- +a +b +c -d = 0
        (show_term  1 "v" $ [i-1, j] ++ free_indices) ++ (show_term   1  "h" $ [i, j-1] ++ free_indices) ++
        (show_term  1 "v" $ [i,   j] ++ free_indices) ++ (show_term (-1) "h" $ [i, j]   ++ free_indices) ++ " >= 0;\n" ++
    
        -- +a +b -c +d = 0
        (show_term   1  "v" $ [i-1, j] ++ free_indices) ++ (show_term 1 "h" $ [i, j-1] ++ free_indices) ++
        (show_term (-1) "v" $ [i,   j] ++ free_indices) ++ (show_term 1 "h" $ [i, j]   ++ free_indices) ++ " >= 0;\n" ++
    
        -- +a -b +c +d = 0
        (show_term 1 "v" $ [i-1, j] ++ free_indices) ++ (show_term (-1) "h" $ [i, j-1] ++ free_indices) ++
        (show_term 1 "v" $ [i,   j] ++ free_indices) ++ (show_term   1  "h" $ [i, j]   ++ free_indices) ++ " >= 0;\n" ++
    
        -- +a -b -c -d = -2
        (show_term   1  "v" $ [i-1, j] ++ free_indices) ++ (show_term (-1) "h" $ [i, j-1] ++ free_indices) ++
        (show_term (-1) "v" $ [i,   j] ++ free_indices) ++ (show_term (-1) "h" $ [i, j]   ++ free_indices) ++ " >= -2;\n" ++
    
        -- -a +b +c +d = 0
        (show_term (-1) "v" $ [i-1, j] ++ free_indices) ++ (show_term 1 "h" $ [i, j-1] ++ free_indices) ++
        (show_term   1  "v" $ [i,   j] ++ free_indices) ++ (show_term 1 "h" $ [i, j]   ++ free_indices) ++ " >= 0;\n" ++
    
        -- -a +b -c -d = -2
        (show_term (-1) "v" $ [i-1, j] ++ free_indices) ++ (show_term   1  "h" $ [i, j-1] ++ free_indices) ++
        (show_term (-1) "v" $ [i,   j] ++ free_indices) ++ (show_term (-1) "h" $ [i, j]   ++ free_indices) ++ " >= -2;\n" ++
    
        -- -a -b +c -d = -2
        (show_term (-1) "v" $ [i-1, j] ++ free_indices) ++ (show_term (-1) "h" $ [i, j-1] ++ free_indices) ++
        (show_term   1  "v" $ [i,   j] ++ free_indices) ++ (show_term (-1) "h" $ [i, j]   ++ free_indices) ++ " >= -2;\n" ++
    
        -- -a -b -c +d = -2
        (show_term (-1) "v" $ [i-1, j] ++ free_indices) ++ (show_term (-1) "h" $ [i, j-1] ++ free_indices) ++
        (show_term (-1) "v" $ [i,   j] ++ free_indices) ++ (show_term   1  "h" $ [i, j]   ++ free_indices) ++ " >= -2;\n" ++
    
        -- -a -b -c -d = -3
        (show_term (-1) "v" $ [i-1, j] ++ free_indices) ++ (show_term (-1) "h" $ [i, j-1] ++ free_indices) ++
        (show_term (-1) "v" $ [i,   j] ++ free_indices) ++ (show_term (-1) "h" $ [i, j]   ++ free_indices) ++ " >= -3;\n"
    
    -- connectivity equations with vias
    connectivity_stencil_vias ::  Bool -> Point -> [Int] -> String
    connectivity_stencil_vias is_term (i,j) free_indices =
      if is_term
      then -- terminal cell
        (show_term 1 "h" $ [i,   j-1] ++ free_indices) ++ (show_term 1 "h" $ [i, j] ++ free_indices) ++ 
        (show_term 1 "v" $ [i-1, j]   ++ free_indices) ++ (show_term 1 "v" $ [i, j] ++ free_indices) ++ " = +1;\n" ++
        (show_term 1 "c" $ [i,   j]   ++ free_indices) ++ " = 0;\n"
      else -- pass cell
    
        -- a = h_{i,j-1} b = h_{i,j} c = v_{i-1,j} d = v_{i,j} e = c_{i,j}
        -- +a +b +c +d -e = 0
        (show_term   1  "h" $ [i, j-1] ++ free_indices) ++ (show_term   1  "h" $ [i, j] ++ free_indices) ++
        (show_term   1  "v" $ [i-1, j] ++ free_indices) ++ (show_term   1  "v" $ [i, j] ++ free_indices) ++ 
        (show_term (-1) "c" $ [i, j]   ++ free_indices) ++ " >= 0;\n" ++
    
        -- +a +b +c -d +e = 0
        (show_term   1  "h" $ [i, j-1] ++ free_indices) ++ (show_term   1  "h" $ [i, j] ++ free_indices) ++
        (show_term   1  "v" $ [i-1, j] ++ free_indices) ++ (show_term (-1) "v" $ [i, j] ++ free_indices) ++ 
        (show_term   1  "c" $ [i, j]   ++ free_indices) ++ " >= 0;\n" ++
    
        -- +a +b +c -d -e = -1
        (show_term   1  "h" $ [i, j-1] ++ free_indices) ++ (show_term   1  "h" $ [i, j] ++ free_indices) ++
        (show_term   1  "v" $ [i-1, j] ++ free_indices) ++ (show_term (-1) "v" $ [i, j] ++ free_indices) ++ 
        (show_term (-1) "c" $ [i, j]   ++ free_indices) ++ " >= -1;\n" ++
    
        -- +a +b -c +d +e = 0
        (show_term   1  "h" $ [i, j-1] ++ free_indices) ++ (show_term   1  "h" $ [i, j] ++ free_indices) ++
        (show_term (-1) "v" $ [i-1, j] ++ free_indices) ++ (show_term   1  "v" $ [i, j] ++ free_indices) ++ 
        (show_term   1  "c" $ [i, j]   ++ free_indices) ++ " >= 0;\n" ++
    
        -- +a +b -c +d -e = -1
        (show_term   1  "h" $ [i, j-1] ++ free_indices) ++ (show_term   1  "h" $ [i, j] ++ free_indices) ++
        (show_term (-1) "v" $ [i-1, j] ++ free_indices) ++ (show_term   1  "v" $ [i, j] ++ free_indices) ++ 
        (show_term (-1) "c" $ [i, j]   ++ free_indices) ++ " >= -1;\n" ++
    
        -- +a +b -c -d -e = -2
        (show_term   1  "h" $ [i, j-1] ++ free_indices) ++ (show_term   1  "h" $ [i, j] ++ free_indices) ++
        (show_term (-1) "v" $ [i-1, j] ++ free_indices) ++ (show_term (-1) "v" $ [i, j] ++ free_indices) ++ 
        (show_term (-1) "c" $ [i, j]   ++ free_indices) ++ " >= -2;\n" ++
    
        -- +a -b +c +d +e = 0
        (show_term   1  "h" $ [i, j-1] ++ free_indices) ++ (show_term (-1) "h" $ [i, j] ++ free_indices) ++
        (show_term   1  "v" $ [i-1, j] ++ free_indices) ++ (show_term   1  "v" $ [i, j] ++ free_indices) ++ 
        (show_term   1  "c" $ [i, j]   ++ free_indices) ++ " >= 0;\n" ++
    
        -- +a -b +c +d -e = -1
        (show_term   1  "h" $ [i, j-1] ++ free_indices) ++ (show_term (-1) "h" $ [i, j] ++ free_indices) ++
        (show_term   1  "v" $ [i-1, j] ++ free_indices) ++ (show_term   1  "v" $ [i, j] ++ free_indices) ++ 
        (show_term (-1) "c" $ [i, j]   ++ free_indices) ++ " >= -1;\n" ++
    
        -- +a -b +c -d +e = -1
        (show_term   1  "h" $ [i, j-1] ++ free_indices) ++ (show_term (-1) "h" $ [i, j] ++ free_indices) ++
        (show_term   1  "v" $ [i-1, j] ++ free_indices) ++ (show_term (-1) "v" $ [i, j] ++ free_indices) ++ 
        (show_term   1  "c" $ [i, j]   ++ free_indices) ++ " >= -1;\n" ++
    
        -- +a -b -c +d +e = -1
        (show_term   1  "h" $ [i, j-1] ++ free_indices) ++ (show_term (-1) "h" $ [i, j] ++ free_indices) ++
        (show_term (-1) "v" $ [i-1, j] ++ free_indices) ++ (show_term   1  "v" $ [i, j] ++ free_indices) ++ 
        (show_term   1  "c" $ [i, j]   ++ free_indices) ++ " >= -1;\n" ++
    
        -- +a -b -c -d +e = -1
        (show_term   1  "h" $ [i, j-1] ++ free_indices) ++ (show_term (-1) "h" $ [i, j] ++ free_indices) ++
        (show_term (-1) "v" $ [i-1, j] ++ free_indices) ++ (show_term (-1) "v" $ [i, j] ++ free_indices) ++ 
        (show_term   1  "c" $ [i, j]   ++ free_indices) ++ " >= -2;\n" ++
    
        -- -a +b +c +d +e = 0
        (show_term (-1) "h" $ [i, j-1] ++ free_indices) ++ (show_term   1  "h" $ [i, j] ++ free_indices) ++
        (show_term   1  "v" $ [i-1, j] ++ free_indices) ++ (show_term   1  "v" $ [i, j] ++ free_indices) ++ 
        (show_term   1  "c" $ [i, j]   ++ free_indices) ++ " >= 0;\n" ++
    
        -- -a +b +c +d -e = -1
        (show_term (-1) "h" $ [i, j-1] ++ free_indices) ++ (show_term   1  "h" $ [i, j] ++ free_indices) ++
        (show_term   1  "v" $ [i-1, j] ++ free_indices) ++ (show_term   1  "v" $ [i, j] ++ free_indices) ++ 
        (show_term (-1) "c" $ [i, j]   ++ free_indices) ++ " >= -1;\n" ++
    
        -- -a +b +c -d +e = -1
        (show_term (-1) "h" $ [i, j-1] ++ free_indices) ++ (show_term   1  "h" $ [i, j] ++ free_indices) ++
        (show_term   1  "v" $ [i-1, j] ++ free_indices) ++ (show_term (-1) "v" $ [i, j] ++ free_indices) ++ 
        (show_term   1  "c" $ [i, j]   ++ free_indices) ++ " >= -1;\n" ++
    
        -- -a +b -c +d +e = -1
        (show_term (-1) "h" $ [i, j-1] ++ free_indices) ++ (show_term   1  "h" $ [i, j] ++ free_indices) ++
        (show_term (-1) "v" $ [i-1, j] ++ free_indices) ++ (show_term   1  "v" $ [i, j] ++ free_indices) ++ 
        (show_term   1  "c" $ [i, j]   ++ free_indices) ++ " >= -1;\n" ++
    
        -- -a +b -c -d +e = -2
        (show_term (-1) "h" $ [i, j-1] ++ free_indices) ++ (show_term   1  "h" $ [i, j] ++ free_indices) ++
        (show_term (-1) "v" $ [i-1, j] ++ free_indices) ++ (show_term (-1) "v" $ [i, j] ++ free_indices) ++ 
        (show_term   1  "c" $ [i, j]   ++ free_indices) ++ " >= -2;\n" ++
    
        -- -a -b +c +d -e = -2
        (show_term (-1) "h" $ [i, j-1] ++ free_indices) ++ (show_term (-1) "h" $ [i, j] ++ free_indices) ++
        (show_term   1  "v" $ [i-1, j] ++ free_indices) ++ (show_term   1  "v" $ [i, j] ++ free_indices) ++ 
        (show_term (-1) "c" $ [i, j]   ++ free_indices) ++ " >= -2;\n" ++
    
        -- -a -b +c -d +e = -2
        (show_term (-1) "h" $ [i, j-1] ++ free_indices) ++ (show_term (-1) "h" $ [i, j] ++ free_indices) ++
        (show_term   1  "v" $ [i-1, j] ++ free_indices) ++ (show_term (-1) "v" $ [i, j] ++ free_indices) ++ 
        (show_term   1  "c" $ [i, j]   ++ free_indices) ++ " >= -2;\n" ++
    
        -- -a -b -c +d +e = -2
        (show_term (-1) "h" $ [i, j-1] ++ free_indices) ++ (show_term (-1) "h" $ [i, j] ++ free_indices) ++
        (show_term (-1) "v" $ [i-1, j] ++ free_indices) ++ (show_term   1  "v" $ [i, j] ++ free_indices) ++ 
        (show_term   1  "c" $ [i, j]   ++ free_indices) ++ " >= -2;\n"
    
        -- ADDITIONAL CONSTRAINTS
        ++
        -- +a -b -c -d -e = -3
        (show_term   1  "h" $ [i, j-1] ++ free_indices) ++ (show_term (-1) "h" $ [i, j] ++ free_indices) ++
        (show_term (-1) "v" $ [i-1, j] ++ free_indices) ++ (show_term (-1) "v" $ [i, j] ++ free_indices) ++ 
        (show_term (-1) "c" $ [i, j]   ++ free_indices) ++ " >= -3;\n" ++
    
        -- -a +b -c -d -e = -3
        (show_term (-1) "h" $ [i, j-1] ++ free_indices) ++ (show_term   1  "h" $ [i, j] ++ free_indices) ++
        (show_term (-1) "v" $ [i-1, j] ++ free_indices) ++ (show_term (-1) "v" $ [i, j] ++ free_indices) ++ 
        (show_term (-1) "c" $ [i, j]   ++ free_indices) ++ " >= -3;\n" ++
    
        -- -a -b +c -d -e = -3
        (show_term (-1) "h" $ [i, j-1] ++ free_indices) ++ (show_term (-1) "h" $ [i, j] ++ free_indices) ++
        (show_term   1  "v" $ [i-1, j] ++ free_indices) ++ (show_term (-1) "v" $ [i, j] ++ free_indices) ++ 
        (show_term (-1) "c" $ [i, j]   ++ free_indices) ++ " >= -3;\n" ++
    
        -- -a -b -c +d -e = -3
        (show_term (-1) "h" $ [i, j-1] ++ free_indices) ++ (show_term (-1) "h" $ [i, j] ++ free_indices) ++
        (show_term (-1) "v" $ [i-1, j] ++ free_indices) ++ (show_term   1  "v" $ [i, j] ++ free_indices) ++ 
        (show_term (-1) "c" $ [i, j]   ++ free_indices) ++ " >= -3;\n" ++
    
        -- -a -b -c -d +e = -3
        (show_term (-1) "h" $ [i, j-1] ++ free_indices) ++ (show_term (-1) "h" $ [i, j] ++ free_indices) ++
        (show_term (-1) "v" $ [i-1, j] ++ free_indices) ++ (show_term (-1) "v" $ [i, j] ++ free_indices) ++ 
        (show_term   1  "c" $ [i, j]   ++ free_indices) ++ " >= -3;\n" ++
    
        -- -a -b -c -d -e = -4
        (show_term (-1) "h" $ [i, j-1] ++ free_indices) ++ (show_term (-1) "h" $ [i, j] ++ free_indices) ++
        (show_term (-1) "v" $ [i-1, j] ++ free_indices) ++ (show_term (-1) "v" $ [i, j] ++ free_indices) ++ 
        (show_term (-1) "c" $ [i, j]   ++ free_indices) ++ " >= -4;\n"



asy_out :: Handle -> TestCase -> IO ()
asy_out h tc =
  do
    let grid_size = grid tc
        nets  = terminals tc
        num_nets = length nets
        kors = keepouts tc
    hPutStrLn h "import route_prims;\n\nsize(200,0);\n"
    hPutStrLn h $ "draw_grid(" ++ (show $ snd grid_size) ++ "," ++ (show $ fst grid_size) ++ ",true);\n"
    let terminals = foldr1 (++) nets
    mapM_ (\(y,x) -> hPutStrLn h $ "draw_term((" ++ (show x) ++ "," ++ (show y) ++ "));") terminals
    hPutStrLn h $ "import patterns;\nadd(\"brick\", brick());\nadd(\"crosshatch\", crosshatch(4));"
    mapM_ (\((lly,llx),(ruy,rux)) -> hPutStrLn h $
           "filldraw ((" ++ (show llx) ++ "-1," ++ (show lly) ++ "-1)--("
                         ++ (show llx) ++ "-1," ++ (show ruy) ++ "-1)--("
                         ++ (show rux) ++ "-1," ++ (show ruy) ++ "-1)--("
                         ++ (show rux) ++ "-1," ++ (show lly) ++ "-1)--cycle, pattern(\"crosshatch\"));"
          ) kors


safeOpenFile :: String -> IOMode -> IO Handle
safeOpenFile name mode =
  catch (openFile name mode)
  (\err -> do putStrLn ("Cannot open " ++ name)
              print err
              -- exitWith (ExitFailure 1)
              putStr "Give me another filename:"
              hFlush stdout
              s <- getLine
              safeOpenFile s mode)

parseCmdLine :: [String] -> IO (String,Int,String) -- returns out filename
parseCmdLine (filename:case_num:[]) = return (filename, read case_num, "") 
parseCmdLine (filename:case_num:solver_name:[]) = return (filename, read case_num, solver_name) 
parseCmdLine (_) = error "filename AND case number should be given"

reportTimeUsage startTime endTime =
  do
    let diffTime = endTime - startTime
        timeInMS = diffTime `div` 1000000000
    putStrLn $ "elapsed time " ++ (show diffTime) ++
             " in microseconds is " ++ (show timeInMS) ++ "ms"

main = do
  putStrLn "Welcome to PB Route!"
  startTime <- getCPUTime

  cmd_line <- getArgs
  (out_fname, case_num, solver_name) <- parseCmdLine cmd_line

  h  <- safeOpenFile out_fname WriteMode
  let basename = takeWhile (/= '.') out_fname
  h1 <- safeOpenFile (basename ++ "_opb.asy") WriteMode

  let print_ast = case solver_name of "pueblo"   -> False
                                      "Pueblo"   -> False
                                      "glpPB"    -> False
                                      "minisat+" -> True
                                      _          -> True

  gen h (test_case!!(case_num-1)) print_ast
  asy_out h1 (test_case!!(case_num-1))


  hClose h
  hClose h1

  endTime <- getCPUTime

  reportTimeUsage startTime endTime

  exitWith ExitSuccess


