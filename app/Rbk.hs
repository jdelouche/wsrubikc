{-# LANGUAGE OverloadedStrings #-}
module Rbk where
import Prelude
import qualified Rainbow as R
import qualified Rainbow.Translate as S
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Internal as BSLazy
import Data.Function ((&))
import Data.Typeable
import Data.ByteString.Lazy.Char8 as Char8
data Color =W|O|Y|R|B|G
instance Show Color where
  show W = "\ESC[0m\ESC[48;5;7;38;5;0;1mW\ESC[0m"
  show O = "\ESC[0m\ESC[48;5;208;38;5;0;1;51mO\ESC[0m"
  show Y = "\ESC[0m\ESC[48;5;11;38;5;0;1mY\ESC[0m"
  show R = "\ESC[0m\ESC[48;5;9;38;5;0;1mR\ESC[0m"
  show B = "\ESC[0m\ESC[48;5;12;38;5;0;1mB\ESC[0m"
  show G = "\ESC[0m\ESC[48;5;10;38;5;0;1mG\ESC[0m"
data Corner=Cnr{cfrt::Color,clft::Color,ctop::Color}
instance Show Corner where
  show Cnr{cfrt=a,clft=b,ctop=c}=(show a) ++ (show b) ++ (show c)
data Center=Ctr{efrt::Color,etop::Color}
instance Show Center where
  show Ctr{efrt=a,etop=b}=(show a) ++ (show b)
data Middle=Mdl{mfrt::Color}
instance Show Middle where
  show Mdl{mfrt=w}=show w
data Edge  =Edg{edglft::Corner,edgctr::Center}
instance Show Edge where
  show Edg{edglft=a,edgctr=b}=show a++"-"++show b
data Face  =Fce{fmdl::Middle,ffrt::Edge,frgt::Edge,fbck::Edge,flft::Edge}
instance Show Face where
 show Fce{fmdl=a,ffrt=b,frgt=c,fbck=d,flft=e}=""
                                            ++"      "++show b++"\n"
                                            ++show e++"  "++show a++"   "++show c++"\n"
                                            ++"      "++show d++"\n"
data Rubik =Rbk{rtop::Face,rfrt::Face,rbtt::Face,rbck::Face}
instance Show Rubik where
  show Rbk{rtop=a,rfrt=b,rbtt=c,rbck=d}=show a++"\n"++show b++"\n"++show c++"\n"++show d
rbk = Rbk {
            rtop= Fce {
                        fmdl=Mdl{ mfrt=W},
                        ffrt=Edg{ 
                                  edglft=Cnr{cfrt=B,clft=W,ctop=O},
                                  edgctr=Ctr{efrt=O,etop=W}
                                },
                        frgt=Edg{ 
                                  edglft=Cnr{cfrt=O,clft=W,ctop=G},
                                  edgctr=Ctr{efrt=G,etop=W}
                                },
                        fbck=Edg{ 
                                  edglft=Cnr{cfrt=G,clft=W,ctop=R},
                                  edgctr=Ctr{efrt=R,etop=W}
                                },
                        flft=Edg{ 
                                  edglft=Cnr{cfrt=R,clft=W,ctop=B},
                                  edgctr=Ctr{efrt=B,etop=W}
                                }
                      },
            rfrt= Fce {
                        fmdl=Mdl{ mfrt=O},
                        ffrt=Edg{ 
                                  edglft=Cnr{cfrt=B,clft=O,ctop=Y},
                                  edgctr=Ctr{efrt=Y,etop=O}
                                },
                        frgt=Edg{ 
                                  edglft=Cnr{cfrt=Y,clft=O,ctop=G},
                                  edgctr=Ctr{efrt=G,etop=O}
                                },
                        fbck=Edg{ 
                                  edglft=Cnr{cfrt=G,clft=O,ctop=W},
                                  edgctr=Ctr{efrt=W,etop=O}
                                },
                        flft=Edg{ 
                                  edglft=Cnr{cfrt=W,clft=O,ctop=B},
                                  edgctr=Ctr{efrt=B,etop=O}
                                }
                      },
            rbtt= Fce {
                        fmdl=Mdl{ mfrt=Y},
                        ffrt=Edg{ 
                                  edglft=Cnr{cfrt=B,clft=Y,ctop=R},
                                  edgctr=Ctr{efrt=R,etop=Y}
                                },
                        frgt=Edg{ 
                                  edglft=Cnr{cfrt=R,clft=Y,ctop=G},
                                  edgctr=Ctr{efrt=G,etop=Y}
                                },
                        fbck=Edg{ 
                                  edglft=Cnr{cfrt=G,clft=Y,ctop=O},
                                  edgctr=Ctr{efrt=O,etop=Y}
                                },
                        flft=Edg{ 
                                  edglft=Cnr{cfrt=O,clft=Y,ctop=B},
                                  edgctr=Ctr{efrt=B,etop=Y}
                                }
                      },
            rbck= Fce {
                        fmdl=Mdl{ mfrt=R},
                        ffrt=Edg{ 
                                  edglft=Cnr{cfrt=B,clft=R,ctop=W},
                                  edgctr=Ctr{efrt=W,etop=R}
                                },
                        frgt=Edg{ 
                                  edglft=Cnr{cfrt=W,clft=R,ctop=G},
                                  edgctr=Ctr{efrt=G,etop=R}
                                },
                        fbck=Edg{ 
                                  edglft=Cnr{cfrt=G,clft=R,ctop=Y},
                                  edgctr=Ctr{efrt=Y,etop=R}
                                },
                        flft=Edg{ 
                                  edglft=Cnr{cfrt=Y,clft=R,ctop=B},
                                  edgctr=Ctr{efrt=B,etop=R}
                                }
                      }
          }
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
