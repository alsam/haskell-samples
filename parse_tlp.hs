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
         eps                                      <- coll flt
         l4@(nh,nx,ny,isr)                        <- four_ints
         conds                                    <- p_conds nconds
         diel_ifaces                              <- p_diel_ifaces ndiel_ifaces eps
         left_bnd                                 <- diel_iface_decl [0,0]
         right_bnd                                <- diel_iface_decl [0,0]
         eof
         return (nconds,l2,eps,l4,conds,diel_ifaces,left_bnd,right_bnd)
 
sngl_int = do { n <- int; eol; return n }

four_ints = do { n1 <- int; n2 <- int; n3 <- int; n4 <- int; eol
               ; return (n1,n2,n3,n4)
               }

coll p = do { v <- many1 p; eol; return v }

pair_flts = do { x <- flt; y <- flt; eol; return (x,y) }

cond_decl = do { a <- flt; b <- flt; xc <- flt; yc <- flt
               ; neps <- int; dxl <- flt; dxr <- flt; inp <- int; eol;
               ; return (a,b,xc,yc,neps,dxl,dxr,inp)
               }

diel_iface_decl (el:er:eps_rest) = do { cnt     <- sngl_int
                                      ; ifc     <- count (cnt+1) pair_flts
                                      ; samples <- coll int
                                      ; return (eps_rest,(el,er),ifc,samples)
                                      }


------------------------------

t_int (x:xs) = do { n<- int; eol; return (xs,n) }

t_sngl list nc = count nc (t_int list)

t_parse list nc inp = parse (t_sngl list nc) "(unk)" inp

------------------------------


p_conds nc = count nc cond_decl

p_diel_ifaces ndi eps = count ndi (diel_iface_decl eps)

test_parse_tlp input = parse tlp "(unknown)" input

parse_tlp fname input = parse tlp fname input

