{-imitation de la fonction sum -}
sum':: Num a=> [a]->a
sum' [] = 0
sum' (x:xs) = x + sum' xs
{-imitation de la fonction product -}
product':: Num a=> [a]->a
product' [] = 1
product' (x:xs) = x * product' xs
{- ecrivont map de trois facons -}
map' _ [] = []
map' f (x:xs)= (f x):map' f xs
map2 f xs = [(f x)|x<-xs]
map3 f xs = foldr (\x acc -> f x : acc) [] xs 
{- ecriture de concat de deux facons -}
concat' [] = []
concat' (x:xs) = x ++ concat' xs
concat2 xs = foldr (\x acc->x++acc) [] xs
{-imlpementons takeWhile dropeWhile-}
takeWhile':: (a->Bool)->[b]->[b]
takeWhile' _ [] = []
takeWihile' p (x:xs)
	| p x       = x : takeWihile' p xs  
	| otherwise = takeWihile' p xs
dropeWhile' _ [] = []
dropeWihile' f (x:xs)
	| (f x) =  dropeWihile' f xs 
	| otherwise = x: dropeWihile' f xs
f [] = []
f (x:xs) = f xs ++ [x]
{-implementation de la fonction (++)-}
c [] ys = ys
c xs [] = xs
c (x:xs) ys = x: c xs ys
{-fonction sorted qui dit si une liste est trie a partir de sorted -}
pairs [] = []
pairs xs = zip xs (tail xs)
sorted [] = True
sorted xs = let ys=pairs xs in sortP ys
	where 
		sortP [] = True
		sortP (x:xs)=if(fst x < snd x) then sortP xs else False 	 
lastButOne [] = error "empty list"
lastButOne (x:[]) = error "more than one element"
lastButOne xs = (reverse xs) !! 1
headButOne [] = error "empty list"
headButOne (x:[]) = error "more than one element"
headButOne (x:y:_) = y
isPalin [] = error "empty list"
isPalin xs = reverse xs == xs
list2palin xs = xs ++ reverse xs 
sortLength [] = []
sortLength (x:xs) = let ys=[y|y<-xs,length y <= length x]; zs=[z|z<-xs,length z > length x] in sortLength ys++[x]++sortLength zs 
