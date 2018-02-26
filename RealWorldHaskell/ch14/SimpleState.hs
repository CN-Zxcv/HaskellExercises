
import System.Random
import Control.Monad
import Control.Monad.Trans.State

type SimpleState s a = s -> (a, s)

type StringState a = SimpleState String a

returnSt :: a -> SimpleState s a
returnSt a = \s -> (a, s)

bindSt :: (SimpleState s a) -> (a -> SimpleState s b) -> SimpleState s b
bindSt m k = \s ->
    let (a, s') = m s
    in (k a) s'

getSt :: SimpleState s s
getSt = \s -> (s, s)

putSt :: s -> SimpleState s ()
putSt s = \_ -> ((), s)

-------


type RandomState a = State StdGen a

getRandom :: Random a => RandomState a
getRandom =
    get >>= \gen ->
    let (val, gen') = random gen
    in
    put gen' >>
    return val

getTwoRandoms :: Random a => RandomState (a, a)
getTwoRandoms = liftM2 (,) getRandom getRandom
