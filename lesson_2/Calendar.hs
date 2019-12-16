module Calendar where

import Prelude (Int, Char, String, Show(..), (++))

-- Дата
data Date = Date Year Month Day

-- Год
data Year  = Year Int       -- Int это целые числа

-- Месяц
data Month  = January    | February   | March    | April          
            | May        | June       | July     | August   
            | September  | October    | November | December

data Day = Day Int

-- Неделя
data Week  = Monday     | Tuesday   | Wednesday 
            | Thursday   | Friday    | Saturday     
            | Sunday   

-- Время
data Time = Time Hour Minute Second

data Hour   = Hour   Int    -- Час
data Minute = Minute Int    -- Минута
data Second = Second Int    -- Секунда

instance Show Week where
    show Monday     = "Mon"
    show Tuesday    = "Tue"
    show Wednesday  = "Wed"
    show Thursday   = "Thu"  
    show Friday     = "Fri" 
    show Saturday   = "Sat"
    show Sunday     = "Sun"

instance Show Time where
    show (Time h m s) = show h ++ ":" ++ show m ++ ":" ++ show s

instance Show Hour where
    show (Hour h) = addZero (show h)

instance Show Minute where
    show (Minute m) = addZero (show m)

instance Show Second where
    show (Second s) = addZero (show s)

addZero :: String -> String
addZero (a:[]) = '0' : a : []
addZero as     = as

instance Show Month where
    show January = "Jan"
    show February = "Feb"
    show March = "Mar"
    show April = "Apr"
    show May = "May"
    show June = "Jun"
    show July = "Jul"
    show August = "Aug"
    show September = "Sep"
    show October = "Oct"
    show November = "Nov"
    show December = "Dec"

instance Show Year where
    show (Year y) = show y

instance Show Day where
    show (Day d) = addZero (show d)

instance Show Date where
    show (Date y m d) = show d ++ " " ++ show m ++ " " ++ show y
