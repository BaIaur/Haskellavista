-- Sinnvolle Bestückung für jede Funktion:
-- a) Was soll sie tun, falls von einem übungsblatt die passende Aufgabe (Blatt+Nummer) dazu schreiben
-- b) "Tests"
{--------------------------------------------------------------------------------------------------------------------------------------}
mymax::Ord a=> [a] -> a
mymax [] = error "no element"
mymax [x] = x
mymax (x:xs) | (mymax xs) > x = mymax xs
             |  otherwise = x

test_mymax = t_mymax1 && t_mymax2 && t_mymax3 && t_mymax4 && t_mymax5
t_mymax1 = mymax [1,2,3,4] == 4
t_mymax2 = mymax [4,3,2,1] == 4
t_mymax3 = mymax [4,4,3,1] == 4
t_mymax4 = mymax [1,1,1] == 1
t_mymax5 = mymax [1] == 1
{--------------------------------------------------------------------------------------------------------------------------------------}
mymin::Ord a=> [a] -> a
mymin [] =  error "no element"
mymin [x] = x
mymin (x:xs) | (mymin xs) < x = mymin xs
             |  otherwise = x

test_mymin = t_mymin1 && t_mymin2 && t_mymin3 && t_mymin4 && t_mymin5
t_mymin1 = mymin [1,2,3,4] == 1
t_mymin2 = mymin [4,3,2,1] == 1
t_mymin3 = mymin [1,1,3,4] == 1
t_mymin4 = mymin [1,1,1] == 1
t_mymin5 = mymin [1] == 1
{--------------------------------------------------------------------------------------------------------------------------------------}
-- ich habe hier mal haskell nach einer signatur gefragt, es sprach sofort: Integer -> Integer -> Integer
-- du kannst also grundsätzlich auch die signatur benutzen statt der vollgenerischen aus Num und Eq
-- myadd::Eq a => a -> a -> a
-- hier könntest du mit einer eigenen data definition arbeiten und dann eine convert funktion von deinem data in richtige zahlen machen. oder operationen in deiner struktur basteln
myadd :: Integer -> Integer -> Integer
myadd 0 a = a
myadd n a = (myadd a (n-1))+1
--nur für natürliche Zahlen
test_myadd = (test_myadd1, test_myadd2, test_myadd3,test_myadd4)

test_myadd1 = myadd 5 5
test_myadd2 = myadd 500 7000
test_myadd3 = myadd 9 0
test_myadd4 = myadd 0 9
{--------------------------------------------------------------------------------------------------------------------------------------}
mysub::Eq a =>Num a=> a -> a -> a
mysub 0 a = a
mysub n a = (mysub a (n-1))-1
--nur für natürliche Zahlen
{--------------------------------------------------------------------------------------------------------------------------------------}
mymul::Eq a=>Num a=> a -> a -> a
mymul a 0 = 0
mymul a n = a+(mymul a (n-1))
--nur für natürliche Zahlen
{--------------------------------------------------------------------------------------------------------------------------------------}
reverse1::Num a => [a] -> [a]
reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++[x]
{--------------------------------------------------------------------------------------------------------------------------------------}
-- schreibt man immer noch lengTH
mylength::Num a=>[a]-> a
mylength [] = 0
mylength xs = 1+mylength xs
{--------------------------------------------------------------------------------------------------------------------------------------}
mysum::Num a=>[a]-> a
mysum [] = 0
mysum (x:xs) = x + mysum xs
{--------------------------------------------------------------------------------------------------------------------------------------}
-- da hätte ich gerne eine komplexitätsanalyse / betrachtung / idee von
primes = sieb [2..]
         where
              sieb (p:xs) = p:sieb[k | k<-xs, (mod k p)>0]

-- ich mein, das kann man machen, aber was ist die motivation davon?
primeOne :: Int -> [Integer]
primeOne n = take n [x | x<-primes, (mod x 10)==1]
{--------------------------------------------------------------------------------------------------------------------------------------}
--map2::(a->b)->[a]->[b]
-- maptastisch
map2 f [] = []
map2 f (x:xs) = f x: (map2 f xs)
{--------------------------------------------------------------------------------------------------------------------------------------}
foldr2::(a -> b -> b) -> b -> [a] -> b
foldr2 f e [] = e
foldr2 f e (x:xs)= f x (foldr2 f e xs)
{--------------------------------------------------------------------------------------------------------------------------------------}
-- was ist das denn für ein monster? 
myadd2::Ord a=>Eq a =>Num a=> a -> a -> a
myadd2 0 a = a
myadd2 n a | a<0 = (myadd2 a (n-1))+1
       | n<0 = (myadd2 (a-1) n)+1
       | n<0 && a<0 =  (myadd2 (a+1) n)+1
       | otherwise =(myadd2 a (n-1))+1
{--------------------------------------------------------------------------------------------------------------------------------------}
add3::Ord a=>Eq a=> Num a=> a-> a ->a
add3 0 a = a
add3 n a | a<0 = n+a -- die beiden zeilen kann man evtl. kombinieren mit oder?
         | n<0 = n+a
         | n<0 && a<0 = n+a -- irgendwie hast du hier keinen mehrwert, das rechnet immer n+a, du fängst ein paar fälle die aber gleiche effekte haben und den rest kippst du dann letztlich auch in die gleiche aktion
         | otherwise = n+a
{--------------------------------------------------------------------------------------------------------------------------------------}
sub3::Ord a=>Eq a=>Num a=> a-> a ->a
sub3 0 a = a
sub3 n a | a<0 = a
         | n<0 = a-n
         | n<0 && a<0 = n-a
         | otherwise = -n-a
{--------------------------------------------------------------------------------------------------------------------------------------}
-- auch wieder arg unübersichtlich und mangelnde beschreibung
mysub2::Ord a=>Eq a =>Num a=> a -> a -> a
mysub2 0 a = a
mysub2 a 0 = a
mysub2 n a | a<0 = 1-(mysub2 a (n-1))
           | n<0 = 1-(mysub2 (a-1) n)
           | n<0 && a<0 = 1-(mysub2 a (n+1))
           | otherwise =1-(mysub2 a (n-1))
{--------------------------------------------------------------------------------------------------------------------------------------}
split::[Int]->[[Int]]
split [] =[]
split (x:xs) = [x]:(split xs)
{--------------------------------------------------------------------------------------------------------------------------------------}
--defekt. unnötiger fehler. unnötiger, offensichtlicher fehler. tststs. ;)
-- dafür sehe ich, dass du Listengeneratoren fest in Dein Herz geschlossen hast. Sehr gut :D
-- Guck dir mal die Dictionary und List Comprehensions von Python an, du wirst sie mögen.
deleteSpaces::[Char] -> [Char]
deleteSpaces [] = []
deleteSpaces xs = [x|x<-xs,x/=' ']

test_deleteSpaces = (test_deleteSpaces1, test_deleteSpaces2, test_deleteSpaces3)

test_deleteSpaces1 = deleteSpaces "H e l l o W o r l d"
test_deleteSpaces2 = deleteSpaces "    a     b        c  "
test_deleteSpaces3 = deleteSpaces "1 2 a g 3 2 l oe"

deleteSpaces2::[Char] -> [Char]
deleteSpaces2 [] = []
deleteSpaces2 (x:xs) = if x/=' ' then x:(deleteSpaces2 xs) else deleteSpaces2 xs

test_deleteSpaces_2 = (test2_deleteSpaces1, test2_deleteSpaces2, test2_deleteSpaces3)

test2_deleteSpaces1 = deleteSpaces2 "H e l l o W o r l d"
test2_deleteSpaces2 = deleteSpaces2 "    a     b        c  "
test2_deleteSpaces3 = deleteSpaces2 "1 2 a g 3 2 l oe"

{--------------------------------------------------------------------------------------------------------------------------------------}
{-Schreiben Sie eine rekursive Funktion, die bei Eingabe einer positiven
ganzen Zahl n alle Zahlen zwischen 1 und n aufsummiert, die durch zwei vorgegebene Zahlen teilbar sind. -}
--sum_mults  2  5  100  =>  550
-- Machs dir generell mal leichter indem du die generische Formulierung raus lässt wo du sie nicht brauchst, in dem Fall tut es auch ein Integer -> Integer völlig.
--sum_mults::Num a=> a -> a -> a-> a
--sum_mults a b c = if mod a c -- mod a c dürfte kein boolsches ergebnis liefern, da wirst du schon noch was dran schreiben müssen damit es als teil eines if-ausdrucks verwendbar wird
--  && mod b c ==0 
-- then [x(+)|x<-[1..c]] else error "Nicht teilbar durch den ersten oder den zweiten Eingabeparameter !" -- Der Listengenerator sieht etwas krude aus.
-- sieht etwas vernünftiger aus. Ich hätte jetzt auch auf Filter + Fold getippt.
--sum_mults:: Integer ->[[Integer]]->[Integer]
--sum_mults n = (foldl + mults) -- da fehlt ein startwert
         --where mults=[k|k<-[1..n], mod n k == 0]
-- ===== SPOILER ====
-- Das hier ist übrigens die lange version. Du kannst alles in eine Zeile schreiben. 
-- Die Frage ist allerdings ob es wenn es heißt "schreiben sie eine rekursive funktion" zulässig ist, dass du alles mit Kombinatoren platt machst
-- Also, da hier unten ist kurze Lösung. Seh zu, dass du mit den paar Kombinatoren gut umgehen kannst und Du Sachen damit schnell zusammenstecken kannst, die sind super nützlich und davon gibts noch mehr...

-- Ansonsten: Bau mir das auch mal Rekursiv. Du kannst gerne die beiden Sachen unten einzeln bauen und dann zusammenstecken. Niemand hat in solchen Fällen gesagt, dass es hübsch sein soll, es soll nur zeiteffizient zuverlässig richtig unter Zeitdruck  erledigt sein und dir Punkte geben.
sum_mults :: Integer -> Integer -> Integer -> Integer
sum_mults limit teiler1 teiler2 = foldl (+) 0 filtered
                                  where
                                       filtered = filter (\x -> (x `mod` teiler1 == 0) && (x `mod` teiler2 == 0)) liste

sum_mults::Integer->Integer->Integer->Integer
sum_mults a b c = foldr(+)0(filter(\x -> ((mod x a)==0&&(mod x b)==0 ))[x|x<-[1..c]])

test_sumMults =(test_sumMults1 ,test_sumMults2 ,test_sumMults3)
						
					
test_sumMults1 = sum_mults 2 5 100
test_sumMults2 = sum_mults 3 7 100
test_sumMults3 = sum_mults 2 3 500
{--------------------------------------------------------------------------------------------------------------------------------------}
-- keine spec, keine tests -> kein kommentar. 
arrow::Eq a=>Num a=> a-> a-> a
arrow k 0 = 1
arrow k n = k*(arrow k (n-1))
{--------------------------------------------------------------------------------------------------------------------------------------}
-- wie unterscheidet sich das von der umsetzung oben?
primes2 = sieb [2..]
        where
        sieb (p:xs) = p:sieb[k | k<-xs, (mod k p)>0]
-- dito siehe oben
mersenne n = take n [ x | x<-primes2]
{--------------------------------------------------------------------------------------------------------------------------------------}
-- also ich hätte hier ja ein ding erwartet was mir strings mit sätzen in eine liste aus wörtern / token zerlegt. das geht und ist eine schöne übung. mach mal.
-- tipp des tages hier: akkumulatoren.
chars2words:: [Char] -> [[Char]]
chars2words [] = [[]]
chars2words (x:xs) =  [[x]|x<-xs,x/=' ']
{--------------------------------------------------------------------------------------------------------------------------------------}
twoComplement::[Int]->[Int]
twoComplement [] = []
twoComplement (x:xs) = if x==0 then 1:(twoComplement xs) else 0:(twoComplement xs)
{--------------------------------------------------------------------------------------------------------------------------------------}
-- kein spec, kein spaß
binom_naiv::Integer -> Integer -> Integer
binom_naiv _ 0 = 1
binom_naiv n k = product [n,n-1..n-k+1] `div` product [1..k]
{--------------------------------------------------------------------------------------------------------------------------------------}
-- dito
binom_naiv2::Integer -> Integer -> Integer
binom_naiv2 _ 0 = 1
binom_naiv2 n k | n==k = 1
        | n > k = (binom_naiv2 (n-1) (k-1)) + (binom_naiv2 (n-1) k)
        |otherwise = 0
{--------------------------------------------------------------------------------------------------------------------------------------}
binomtest::Integer -> Integer -> Bool
binomtest n k = (binom_naiv n k == binom_naiv2 n k)
{--------------------------------------------------------------------------------------------------------------------------------------}
divisors::Integer->[Integer]
divisors 0 = []
divisors n = [ k | k<-[1..n], mod n k == 0]
{--------------------------------------------------------------------------------------------------------------------------------------}
-- kein spec, kein test, kein spaß
trueDivisors::[(Integer, [Integer])]
trueDivisors = map2(\a -> (a,divs)) num
          where
          (y:num) = [1..]
          (x:divs) = [ k | k<-[1..], mod x y == 0]
{--------------------------------------------------------------------------------------------------------------------------------------}
-- kein spec, kein test, kein spaß
newPrimes::[Integer]
newPrimes = sieb [2..]
         where
         sieb (p:xs) = p:sieb[ x | x<-xs,(mod x p)>0]
{--------------------------------------------------------------------------------------------------------------------------------------}
-- huh?
allFriendsSmaller n | sum(friends n)< n = (sum(friends n)):[]
             where
             friends n =  [k|k<-[1..n], mod n k == 0]
 -- noch nicht funktional da ich den dreh noch nicht rausgefunden hab 
{--------------------------------------------------------------------------------------------------------------------------------------}
take2 :: Int -> [Int] -> [Int]
take2 n [] = []
take2 n (x:xs)| n == 0 = []
			  | n > 0 = x:(take2 (n-1) xs)
			  | otherwise = []
{--------------------------------------------------------------------------------------------------------------------------------------}				
filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p [] = []
filter2 p (x:xs) | p x       = x : filter2 p xs
				 | otherwise =     filter2 p xs
{--------------------------------------------------------------------------------------------------------------------------------------}			 
zipWith2 :: (a->b->c) -> [a]->[b]->[c]
zipWith2 f [] xs    = []
zipWith2 f ys []    = []
zipWith2 f (y:ys) (x:xs) = f y x : zipWith2 f ys xs
{--------------------------------------------------------------------------------------------------------------------------------------}
--isSorted::(a -> a) -> a-> [Integer] -> Bool
isSorted f  []  = True  --error "nothing in the list which could count as ''sorted''"
isSorted f  [_] = True
isSorted f  xs = (foldr2 f xs)
-- noch nicht funktional versuch-> ich wollte versuchen f als eingabeparameter für fold zu definieren in der isSorted funktion(Z.04)
{--------------------------------------------------------------------------------------------------------------------------------------}
import Data.List
difflist (xs)(ys) = (xs \\ ys)
{--------------------------------------------------------------------------------------------------------------------------------------}
chars2words2 :: Eq a => a -> [a] -> [[a]]
chars2words2 delimter [] = []
chars2words2 delimter zeichen = x : chars2words2 delimter (drop 1 y) 
				where 
				(x,y) = span (/= delimter) zeichen
{--------------------------------------------------------------------------------------------------------------------------------------}
