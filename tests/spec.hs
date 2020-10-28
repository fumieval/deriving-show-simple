import Control.Monad (unless)
import Data.Proxy
import Data.Functor.Identity
import Deriving.Show.Simple
import System.Exit (exitFailure)
import Test.HUnit

identity :: Identity Int
identity = Identity 42

tuple :: (Int, Identity Int)
tuple = (0, identity)

main :: IO ()
main = do
  count <- runTestTT $ TestList
    [ showsPrecSimple 11 Proxy "" ~?= "Proxy"
    , showsPrecSimple 1 Proxy "" ~?= "Proxy"
    , showsPrecSimple 11 identity "" ~?= "(Identity 42)"
    , showsPrecSimple 1 identity "" ~?= "Identity 42"
    , showsPrecSimple 11 tuple "" ~?= "((,) 0 (Identity 42))"
    , showsPrecSimple 1 tuple "" ~?= "(,) 0 (Identity 42)"
    ]
  unless (failures count == 0 && errors count == 0) exitFailure