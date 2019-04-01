-- -*- Haskell -*-

---------------------------------------------------------------------
--  Created:            Tuesday, January 06, 2010
--  Original author:    Alexander Samoilov
---------------------------------------------------------------------

module ParseTLP (parse_tlp) where

import Text.ParserCombinators.Parsec

-- whitespace w/o end of line
ws = " \v\f\t"

-- whitespace with end of line
ws1 =  " \v\f\t\n"
--ws1 = ws ++ "\n"

word = many1 $ noneOf ws1

sp = skipMany $ oneOf ws

int = do { sp; w <- word; sp; return (read w :: Int) }

flt = do { sp; w <- word; sp; return (read w :: Float) }

eol  =  char '\n'
    <?> "end of line"

tlp = do nconds                                   <- sngl_int
         l2@(ndiel_ifaces,nground_planes,isl,isr) <- four_ints
         eps                                      <- coll_cnt (ndiel_ifaces+1) flt
         l4@(nh,nx,ny,nd)                         <- four_ints
         conds                                    <- p_conds nconds
         diel_ifaces                              <- p_diel_ifaces ndiel_ifaces
         left_bnd                                 <- count isl diel_iface_decl
         right_bnd                                <- count isr diel_iface_decl
         eof
         return (nconds,l2,eps,l4,conds,diel_ifaces,left_bnd,right_bnd)
 
sngl_int = do { n <- int; eol; return n }

four_ints = do { n1 <- int; n2 <- int; n3 <- int; n4 <- int; eol
               ; return (n1,n2,n3,n4)
               }

coll p = do { v <- many1 p; eol; return v }

coll_cnt cnt p = do { v <- count cnt p; eol; return v }

pair_flts = do { x <- flt; y <- flt; eol; return (x,y) }

cond_decl = do { a <- flt; b <- flt; xc <- flt; yc <- flt
               ; neps <- int; dxl <- flt; dxr <- flt; inp <- int; eol;
               ; return (a,b,xc,yc,neps,dxl,dxr,inp)
               }

diel_iface_decl = do { cnt     <- sngl_int
                     ; ifc     <- count (cnt+1) pair_flts
                     ; samples <- coll int
                     ; return (ifc,samples)
                     }

p_conds nc = count nc cond_decl

p_diel_ifaces ndi = count ndi diel_iface_decl

parse_tlp fname input = parse tlp fname input

