{-# OPTIONS_GHC -F -pgmF htfpp #-}
module CheckParsers where


import Test.Framework

import {-@ HTF_TESTS @-} System.Directory.Archivemount.VersionParserTest

main :: IO()
main = htfMain htf_importedTests
