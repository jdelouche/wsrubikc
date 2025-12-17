{-# LANGUAGE OverloadedStrings #-}
import Prelude
import Control.Exception
import Data.Typeable
import Rbk
import TplRbk
m = Mdl{mfrt=W}
ct = Ctr{efrt=W,etop=O}
cd = Cnr{cfrt=W,clft=O,ctop=B}
main = do 
       print rbk
       testWhite                     w
       testOrange                 (o w)
       testYellow              (y (o w))
       testRed              (r (y (o w)))
       testBlue  (b (o w) w (r (y (o w))) (y (o w)))
       testGreen (g (o w) w (r (y (o w))) (y (o w)))
       --let rbk2 = (("white",w),("orange",o),("yellow",y),("red",r),("blue",b),("green",g))
       --print rbk2
testGreen g = do
             putStr (assert ((ft (frt g)) == "owg") "")
             putStr (assert ((sd (frt g)) == "go" ) "")
             putStr (assert ((td (frt g)) == "yog") "")
             putStr (assert ((ft (rgt g)) == "yog") "")
             putStr (assert ((sd (rgt g)) == "gy" ) "")
             putStr (assert ((td (rgt g)) == "ryg") "")
             putStr (assert ((ft (bac g)) == "ryg") "")
             putStr (assert ((sd (bac g)) == "gr" ) "")
             putStr (assert ((td (bac g)) == "gwr") "")
             putStr (assert ((ft (lft g)) == "gwr") "")
             putStr (assert ((sd (lft g)) == "gw" ) "")
             putStr (assert ((td (lft g)) == "owg") "")
testBlue b = do
             putStr (assert ((ft (frt b)) == "ybo") "")
             putStr (assert ((sd (frt b)) == "bo" ) "")
             putStr (assert ((td (frt b)) == "obw") "")
             putStr (assert ((ft (rgt b)) == "obw") "")
             putStr (assert ((sd (rgt b)) == "bw" ) "")
             putStr (assert ((td (rgt b)) == "rwb") "")
             putStr (assert ((ft (bac b)) == "rwb") "")
             putStr (assert ((sd (bac b)) == "br" ) "")
             putStr (assert ((td (bac b)) == "rby") "")
             putStr (assert ((ft (lft b)) == "rby") "")
             putStr (assert ((sd (lft b)) == "by" ) "")
             putStr (assert ((td (lft b)) == "ybo") "")
testRed  r = do
             putStr (assert ((ft (frt r)) == "rwb") "")
             putStr (assert ((sd (frt r)) == "rw" ) "")
             putStr (assert ((td (frt r)) == "gwr") "")
             putStr (assert ((ft (rgt r)) == "gwr") "")
             putStr (assert ((sd (rgt r)) == "gr" ) "")
             putStr (assert ((td (rgt r)) == "ryg") "")
             putStr (assert ((ft (bac r)) == "ryg") "")
             putStr (assert ((sd (bac r)) == "ry" ) "")
             putStr (assert ((td (bac r)) == "rby") "")
             putStr (assert ((ft (lft r)) == "rby") "")
             putStr (assert ((sd (lft r)) == "br" ) "")
             putStr (assert ((td (lft r)) == "rwb") "")
testYellow y = do
               putStr (assert ((ft (frt y)) == "rby") "")
               putStr (assert ((sd (frt y)) == "ry" ) "")
               putStr (assert ((td (frt y)) == "ryg") "")
               putStr (assert ((ft (rgt y)) == "ryg") "")
               putStr (assert ((sd (rgt y)) == "gy" ) "")
               putStr (assert ((td (rgt y)) == "yog") "")
               putStr (assert ((ft (bac y)) == "yog") "")
               putStr (assert ((sd (bac y)) == "yo" ) "")
               putStr (assert ((td (bac y)) == "ybo") "")
               putStr (assert ((ft (lft y)) == "ybo") "")
               putStr (assert ((sd (lft y)) == "by" ) "")
               putStr (assert ((td (lft y)) == "rby") "")
testOrange o = do
               putStr (assert ((ft (frt o)) == "ybo") "")
               putStr (assert ((sd (frt o)) == "yo" ) "")
               putStr (assert ((td (frt o)) == "yog") "")
               putStr (assert ((ft (rgt o)) == "yog") "")
               putStr (assert ((sd (rgt o)) == "go" ) "")
               putStr (assert ((td (rgt o)) == "owg") "")
               putStr (assert ((ft (bac o)) == "owg") "")
               putStr (assert ((sd (bac o)) == "ow" ) "")
               putStr (assert ((td (bac o)) == "obw") "")
               putStr (assert ((ft (lft o)) == "obw") "")
               putStr (assert ((sd (lft o)) == "bo" ) "")
               putStr (assert ((td (lft o)) == "ybo") "")
testWhite w = do
              putStr (assert ((ft (frt w)) == "obw") "")
              putStr (assert ((sd (frt w)) == "ow" ) "")
              putStr (assert ((td (frt w)) == "owg") "")
              putStr (assert ((ft (rgt w)) == "owg") "")
              putStr (assert ((sd (rgt w)) == "gw" ) "")
              putStr (assert ((td (rgt w)) == "gwr") "")
              putStr (assert ((ft (bac w)) == "gwr") "")
              putStr (assert ((sd (bac w)) == "rw" ) "")
              putStr (assert ((td (bac w)) == "rwb") "")
              putStr (assert ((ft (lft w)) == "rwb") "")
              putStr (assert ((sd (lft w)) == "bw" ) "")
              putStr (assert ((td (lft w)) == "obw") "")
