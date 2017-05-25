module Homework7.StringBufEditor where

import Homework7.StringBuffer
import Homework7.Editor

heditor :: IO()
heditor   = runEditor editor $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
