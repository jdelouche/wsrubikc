{-# LANGUAGE OverloadedStrings #-}
module TplRbk where
import Prelude
ft (x,y,z) =x
sd (x,y,z) =y
td (x,y,z) =z
frt (x,y,z,_,_,_,_,_) = (x,y,z)
rgt (_,_,x,y,z,_,_,_) = (x,y,z)
bac (_,_,_,_,x,y,z,_) = (x,y,z)
lft (z,_,_,_,_,_,x,y) = (x,y,z)
w  =("obw",      "ow",       "owg",   "gw","gwr",      "rw",       "rwb",   "bw")
o w=("ybo",      "yo",       "yog",   "go",td (frt w),sd (frt w),ft (frt w),"bo")
y o=("rby",      "ry",       "ryg",   "gy",td (frt o),sd (frt o),ft (frt o),"by")
r y=(td (bac w),sd (bac w),ft (bac w),"gr",td (frt y),sd (frt y),ft (frt y),"br")
b o w r y=(td (lft o),sd (lft o),ft (lft o),sd (lft w),ft (lft w),sd (lft r),ft (lft r),sd (lft y))
g o w r y=(td (rgt o),sd (rgt o),ft (rgt o),sd (rgt y),ft (rgt y),sd (rgt r),ft (rgt r),sd (rgt w))
rot w = (ft (lft w),sd (lft w), td (lft w),sd (frt w),td (frt w),sd (rgt w),td (rgt w),sd (bac w))
