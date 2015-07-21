even' :: Integer -> Bool
even' x = x `mod` 2 == 0

odd' :: Integer -> Bool
odd' x = x `mod` 2 /= 0

bmi :: Double -> Double -> Double
bmi height weight = weight / (height ^ 2)

deg2rad, rad2deg :: Double -> Double
deg2rad degrees = (degrees / 180) * pi

rad2deg radians = (radians * 180) / pi

isTriangle :: Double -> Double -> Double -> Bool
isTriangle a b c = (a < (b + c)) && (b < (a + c)) && (c < (a + b)) && (a > 0) && (b > 0) && (c > 0)

perimeter, halfPerimeter :: Double -> Double -> Double -> Double
perimeter a b c = if (isTriangle a b c) then a + b + c else error "Not a valid triangle"

halfPerimeter a b c = perimeter a b c / 2

area a b c = sqrt ((halfPerimeter a b c)*(halfPerimeter a b c - a)*(halfPerimeter a b c - b)*(halfPerimeter a b c - c))

calculate sign x y = if sign == '+'
					 then x + y else
					 if sign == '-'
					 then x - y else
					 if sign == '*'
					 then x * y else
					 error "Unknown operator!"

convert "bgn" "usd" value = value * 0.57
convert "usd" "bgn" value = value * 1.74
convert _     _     value = error "Unknown currencies!"

head' (x:_) = x

tail' (_:xs) = xs

last' [] = error "No, no, not an empty string!"
last' (x:[]) = x
last' (x:xs) = last' xs

double [x] = [x]
double (x:xs) = x*2:double xs

mult m [x] = [x]
mult m (x:xs) = x*m:mult m xs

nth 0 (x:_) = x
nth n (x:xs) | n < 0 = error "Invalid index!"
			 | otherwise = nth (n - 1) xs

member m  [] = False
member m (x:xs) | x == m = True
				| otherwise = member m xs

isFibonacciSequence :: [Integer] -> Bool
isFibonacciSequence [] = False
isFibonacciSequence (x:y:xs) | x == 1 = isFibonacciSequence (0:xs)
						     | x + y == head xs = isFibonacciSequence(y:xs)
						     | x + y /= head xs = False
						     | xs == [] = True