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
getX (Canon (_, _, x, _, _)) = x

getAngle :: Canon (Int, Int, Int, Int, Int) -> Int
getAngle (Canon (_, _, _, _, angle)) = angle

getLife :: Canon (Int, Int, Int, Int, Int) -> Int
getLife (Canon (life, _, _, _, _)) = life

getFuel :: Canon (Int, Int, Int, Int, Int) -> Int
getFuel (Canon (_, fuel, _, _, _)) = fuel

getY :: Canon (Int, Int, Int, Int, Int) -> Int
getY (Canon (_, _, _, y, _)) = y

moveX :: Int -> (Int, Int, Int, Int, Int) -> Canon (Int, Int, Int, Int, Int)
moveX deltaX (life, fuel, x, y, angle) = Canon (life, fuel, x + deltaX, y, angle)

moveAngle :: Int -> (Int, Int, Int, Int, Int) -> Canon (Int, Int, Int, Int, Int)
moveAngle deltaAngle (life, fuel, x, y, angle) = Canon (life, fuel, x, y, angle + deltaAngle)

calculaDaño :: StdGen -> Int
calculaDaño gen
    | fst prob <= 5 = fst (randomR (7,9) (snd prob) :: (Int, StdGen))
    | otherwise = fst (randomR (1,3) (snd prob) :: (Int, StdGen))
    where prob = randomR (1,100) gen :: (Int, StdGen)

impactar :: StdGen -> (Int, Int) -> (Canon (Int, Int))
impactar gen (vida, combustible)
    | vida - daño > 0 = (Canon ((vida - daño), combustible)) 
    | otherwise = Dead
    where daño = calculaDaño gen





