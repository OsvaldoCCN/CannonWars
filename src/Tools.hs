module Tools where
import System.Random

data Canon a = Canon a | Dead deriving (Show)

instance Functor Canon where
    fmap f (Canon a) = (Canon (f a))
    fmap f Dead = Dead

instance Applicative Canon where
    pure x = Canon x
    Dead <*> _ = Dead
    (Canon f) <*> (Canon x) = Canon (f x)

instance Monad Canon where
    return x = (Canon x)
    Dead >>= f = Dead
    (Canon x) >>= f = f x

getX :: Canon (Int, Int, Int, Int, Int) -> Int
getX (Dead) = 0
getX (Canon (_, _, x, _, _)) = x

getAngle :: Canon (Int, Int, Int, Int, Int) -> Int
getAngle (Dead) = 0
getAngle (Canon (_, _, _, _, angle)) = angle

getLife :: Canon (Int, Int, Int, Int, Int) -> Int
getLife (Dead) = 0
getLife (Canon (life, _, _, _, _)) = life

getFuel :: Canon (Int, Int, Int, Int, Int) -> Int
getFuel Dead = 0
getFuel (Canon (_, fuel, _, _, _)) = fuel

getY :: Canon (Int, Int, Int, Int, Int) -> Int
getY (Dead) = 0
getY (Canon (_, _, _, y, _)) = y

moveX :: Int -> (Int, Int, Int, Int, Int) -> Canon (Int, Int, Int, Int, Int)
moveX deltaX (life, fuel, x, y, angle) = Canon (life, fuel-8, x + deltaX, y, angle)

moveAngle :: Int -> (Int, Int, Int, Int, Int) -> Canon (Int, Int, Int, Int, Int)
moveAngle deltaAngle (life, fuel, x, y, angle) = Canon (life, fuel-8, x, y, angle + deltaAngle)

calculaDaño :: StdGen -> Int
calculaDaño gen
    | fst prob <= 5 = fst (randomR (7,9) (snd prob) :: (Int, StdGen))
    | otherwise = fst (randomR (1,3) (snd prob) :: (Int, StdGen))
    where prob = randomR (1,100) gen :: (Int, StdGen)

impactar :: StdGen -> (Int, Int, Int, Int, Int) -> (Canon (Int, Int, Int, Int, Int))
impactar gen (vida, f, x, y, a)
    | vida - daño > 0 = (Canon ((vida - daño), f, x, y, a)) 
    | otherwise = Dead
    where daño = calculaDaño gen

resetFuel :: Canon (Int, Int, Int, Int, Int) -> Canon (Int, Int, Int, Int, Int)
resetFuel Dead = Dead
resetFuel (Canon (v,f,x,y,a)) = Canon (v, 100, x, y, a)

