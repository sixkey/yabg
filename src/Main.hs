module Main where 

import Yabg

main :: IO ()
main = do print "yabg"
          yabgPipeline $ YabgSettings { srcPath = "tst"
                                      , dstPath = "bin"
                                      , dirsToCopy = [ "tst/public" ]
                                      , defLinks = [ "/public/index.css" ]
                                      , nav = [ ( "home", "/" )
                                              , ( "blog", "/blog" )
                                              ] }
