-- -*- Haskell -*-

---------------------------------------------------------------------
--  Created:            Tuesday, January 05, 2010
--  Original author:    Alexander Samoilov
---------------------------------------------------------------------

module Main where

import System.IO
import Control.Monad (when)
import System.Exit
import System.Environment (getArgs)
import qualified Data.Map as M

import ParseTLP (parse_tlp)

type Point = (Rational,Rational)

--type Segment = (Point,Point,(Float,Float),Int)

data Segment = Segment {

  -- starting and ending points of the segment
  begin, end :: Point,

  -- dielectric constants to the left and to the right
  eps_l, eps_r :: Float,

  -- cond_no == 0 - diel-diel interface
  cond_no :: Int

} deriving (Show)

main = do
  -- load the command-line arguments
  args <- getArgs

  -- if we don't have the right amount of args, give an error
  when (length args /= 2) $ do
    putStrLn "Syntax: geom_preprocess infile outfile" 
    exitFailure

  let fname = args !! 0
  
  -- read the file lazily
  content <- readFile (args !! 0)

  let tlp_parsed_content = parse_tlp fname content
  let parse_result@(nconds,l2,eps,l4,conds,diel_ifaces,left_bnd,right_bnd) =
        case tlp_parsed_content of
          Left err -> error (show err)
          Right parsed -> parsed

  let cond_segs = gen_cond_segments conds

  let diel_segs = gen_diel_segments eps diel_ifaces

  --putStrLn $ "geom_preprocess finished" ++ (show parse_result)
  putStrLn $ "parse_result nconds: " ++ (show nconds) ++ "\neps: " ++ (show eps)
           ++ "\ncond segments: " ++ (show cond_segs)
           ++ "\ndiel segments: " ++ (show diel_segs)

scale_factor = 10000.0

scale val = toInteger (round $ scale_factor * val)

to_rational val = (toRational (scale val)) / (toRational scale_factor)

rat_point (x,y) = (to_rational x, to_rational y)

gen_cond_segments conds = loop 1 conds
  where loop cond_no (cond:conds) = (gen_segs cond_no cond) ++ (loop (cond_no + 1) conds)
        loop _       []           = []
        gen_segs cond_no (a,b,xc,yc,neps,dxl,dxr,inp) =
          let p1 = rat_point (xc - 0.5*a - dxl, yc - 0.5*b)
              p2 = rat_point (xc + 0.5*a + dxr, yc - 0.5*b)
              p3 = rat_point (xc + 0.5*a - dxr, yc + 0.5*b)
              p4 = rat_point (xc - 0.5*a + dxl, yc + 0.5*b)
          in  Segment { begin = p1, end = p2, eps_l = 0.0, eps_r = 0.0, cond_no = cond_no }
            : Segment { begin = p2, end = p3, eps_l = 0.0, eps_r = 0.0, cond_no = cond_no }
            : Segment { begin = p3, end = p4, eps_l = 0.0, eps_r = 0.0, cond_no = cond_no }
            : Segment { begin = p4, end = p1, eps_l = 0.0, eps_r = 0.0, cond_no = cond_no } : []

gen_diel_segments (eps_l:eps_r:eps) (diface:difaces) =
  (gen_segs eps_l eps_r (fst diface)) ++ gen_diel_segments (eps_r:eps) difaces
  where
    gen_segs eps_l eps_r (p1:p2:px) = Segment { begin = rat_point p1, end = rat_point p2,
                                                eps_l = eps_l, eps_r = eps_r, cond_no = 0 }
                                              : (gen_segs eps_l eps_r (p2:px))
    gen_segs _     _     (p:[])     = []

gen_diel_segments _ [] = []


