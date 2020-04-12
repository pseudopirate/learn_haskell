data Nat = Zero | Succ Nat deriving Show

data Stream a = a :& Stream a

nats :: Nat -> Stream Nat
nats a = a :& nats (Succ a) 

constStream :: a -> Stream a
constStream a = a :& constStream a 

-- Первый элемент потока
headStream :: Stream a -> a
headStream (a :& _) = a

-- Хвост потока, всё кроме первого элемента
tailStream :: Stream a -> Stream a
tailStream (_ :& as) = as

-- n-тый элемент потока
pickStream :: Stream a -> Int -> a
pickStream (a :& _) 0 = a
pickStream (a :& as) n = if n <= 0 then a else pickStream as (n - 1)

-- Берёт из потока несколько первых элементов:
takeStream :: Int -> Stream a -> [a]
takeStream n _ | n < 0 = []
takeStream n (a :& as) = a : takeStream (n - 1) as

instance Show a => Show (Stream a) where
    show xs =  showInfinity (show (takeStream 5 xs))
        where showInfinity x = init x  ++ "..."

iterateStream :: (a -> a) -> a -> Stream a
iterateStream f a = a :& iterateStream f (f a)

-- Преобразование потока
mapStream :: (a -> b) -> Stream a -> Stream b
mapStream f (a :& as) = f a :& mapStream f as

-- Фильтрация потока
-- filter :: (a -> Bool) -> Stream a -> Stream a
-- 
-- zip-ы для потоков:
-- zip :: Stream a -> Stream b -> Stream (a, b)

-- zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c 
