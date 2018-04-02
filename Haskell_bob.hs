mymax::Ord a=> [a] -> a
mymax [] = error "no element"
mymax [x] = x
mymax (x:xs) | (mymax xs) > x = mymax xs
			       |  otherwise = x
			
mYmin::Ord a=> [a] -> a
mYmin [] =  error "no element"
mYmin [x] = x
mYmin (x:xs) | (mYmin xs) < x = mYmin xs
			       |  otherwise = x
				
myadd::Eq a =>Num a=> a -> a -> a
myadd 0 a = a
myadd n a = (myadd a (n-1))+1
--nur für natürliche Zahlen

mysub::Eq a =>Num a=> a -> a -> a
mysub 0 a = a
mysub n a = (mysub a (n-1))-1
--nur für natürliche Zahlen

mymul::Eq a=>Num a=> a -> a -> a
mymul a 0 = 0
mymul a n = a+(mymul a (n-1))
--nur für natürliche Zahlen

reverse1::Num a => [a] -> [a]
reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++[x]

mylenght::Num a=>[a]-> a
mylenght [] = 0
mylenght xs = 1+mylenght xs

mysum::Num a=>[a]-> a
mysum [] = 0
mysum (x:xs) = x + mysum xs

primes = sieb [2..]               
		where                    
		sieb (p:xs) = p:sieb[k | k<-xs, (mod k p)>0] 
		
primeOne :: Int -> [Integer]       
primeOne n = take n [x|x<-primes, (mod x 10)==1]

--map2::(a->b)->[a]->[b]
map2 f [] = []
map2 f (x:xs) = f x: (map2 f xs)

foldr2::(a -> b -> b) -> b -> [a] -> b
foldr2 f e [] = e
foldr2 f e (x:xs)= f x (foldr2 f e xs)

myadd2::Ord a=>Eq a =>Num a=> a -> a -> a
myadd2 0 a = a
myadd2 n a | a<0 = (myadd2 a (n-1))+1
		       | n<0 = (myadd2 (a-1) n)+1
		       | n<0 && a<0 =  (myadd2 (a+1) n)+1
		       | otherwise =(myadd2 a (n-1))+1
		   
add3::Ord a=>Eq a=> Num a=> a-> a ->a
add3 0 a = a
add3 n a | a<0 = n+a
		     | n<0 = n+a
		     | n<0 && a<0 = n+a
		     | otherwise = n+a

sub3::Ord a=>Eq a=>Num a=> a-> a ->a
sub3 0 a = a
sub3 n a | a<0 = a
		     | n<0 = a-n
		     | n<0 && a<0 = n-a
		     | otherwise = -n-a
		
mysub2::Ord a=>Eq a =>Num a=> a -> a -> a
mysub2 0 a = a
mysub2 a 0 = a
mysub2 n a | a<0 = 1-(mysub2 a (n-1))
		       | n<0 = 1-(mysub2 (a-1) n)
		       | n<0 && a<0 = 1-(mysub2 a (n+1))
		       | otherwise =1-(mysub2 a (n-1))
		   
--Betrachten Sie folgende Funktionsdefinition, die eine beliebig lange Liste von Primzahlen berechnet: 
--primes = sieb [2..]               
			--where                    
			--sieb (p:xs) = p:sieb[k | k<-xs, (mod k p)>0] 
--Programmieren Sie damit eine primeOne Funktion, die die ersten n Primzahlen berechnet, die mit der Ziffer 1 enden. 
--Anwendungsbeispiel:    primeOne  6  =>  [11, 31, 41, 61, 71, 101] 

primeOne2::Int->[Integer]
primeOne2 n = take n [x|x<-primes,(mod x 10) ==1]


map3::(a->b)->[a]->[b]
map3 f [] = []
map3 f (x:xs) = f x :(map2 f xs)

--dropwhiley::(a -> Bool) -> [a] -> [a]
split::[Int]->[[Int]]
split [] =[]
split (x:xs) = [x]:(split xs)

deleteSpaces::[Char] -> [Char]
deleteSpaces [] = []
deleteSpaces (x:xs) = [x|x<-xs,x/=' ']

deleteSpaces2::[Char] -> [Char]
deleteSpaces2 [] = []
deleteSpaces2 (x:xs) = if x/=' ' then x:(deleteSpaces2 xs) else deleteSpaces2 xs

{-Schreiben Sie eine rekursive Funktion, die bei Eingabe einer positiven 
ganzen Zahl n alle Zahlen zwischen 1 und n aufsummiert, die durch zwei vorgegebene Zahlen teilbar sind. -}
--sum_mults  2  5  100  =>  550 

--sum_mults::Num a=> a -> a -> a-> a
--sum_mults a b c = if mod a c && mod b c ==0 then [x(+)|x<-[1..c]] else error "Nicht teilbar durch den ersten oder den zweiten Eingabeparameter !"


--sum_mults:: Integer ->[[Integer]]->[Integer]
--sum_mults n = (foldl + mults)
						--where mults=[k|k<-[1..n], mod n k == 0]

arrow::Eq a=>Num a=> a-> a-> a
arrow k 0 = 1
arrow k n = k*(arrow k (n-1))	
	
primes2 = sieb [2..]               
			where                    
			sieb (p:xs) = p:sieb[k | k<-xs, (mod k p)>0]
			
mersenne n = take n [x|x<-primes2]

chars2words:: [Char] -> [[Char]]
chars2words [] = [[]]
chars2words (x:xs) =  [[x]|x<-xs,x/=' ']

twoComplement::[Int]->[Int]
twoComplement [] = []
twoComplement (x:xs) = if x==0 then 1:(twoComplement xs) else 0:(twoComplement xs)

binom_naiv::Integer -> Integer -> Integer
binom_naiv _ 0 = 1
binom_naiv n k = product [n,n-1..n-k+1] `div` product [1..k]
					
binom_naiv2::Integer -> Integer -> Integer
binom_naiv2 _ 0 = 1
binom_naiv2 n k | n==k = 1
				| n > k = (binom_naiv2 (n-1) (k-1)) + (binom_naiv2 (n-1) k)
				|otherwise = 0

binomtest::Integer -> Integer -> Bool
binomtest n k = (binom_naiv n k == binom_naiv2 n k)

divisors::Integer->[Integer]
divisors 0 = []
divisors n = [k|k<-[1..n], mod n k == 0]

trueDivisors::[(Integer, [Integer])]
trueDivisors = map2(\a -> (a,divs)) num
									where
									(y:num) = [1..]
									(x:divs) = [k|k<-[1..], mod x y == 0]

newPrimes::[Integer]
newPrimes = sieb [2..]
			where
			sieb (p:xs) = p:sieb[x|x<-xs,(mod x p)>0]

allFriendsSmaller n | sum(friends n)< n = (sum(friends n)):[]
						where
						friends n =  [k|k<-[1..n], mod n k == 0]



					  
