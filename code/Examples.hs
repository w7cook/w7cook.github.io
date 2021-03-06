
f(x) = x * 2

test1 = let x = 3 in 2*x + 5

test2 = 2 * (let x = 3 in x + 5)

test3 = let x = 3 in let y = x*2 in x + y

test4 = let x = 3 in (let y = x*2 in x + y)

test5 = let x = 2 in
  let y = x+1 in
    let z = y+2 in
      x*y*z

test6 = let x = 9 in (let x = x*x in x+x)

test7 = let x = 3 in
  (let y = 3*x in 2+y) + (let z = 7*x in 1+z)


f'1(x) = x * 2
f'2 x  = x * 2
f'3 = \x -> x * 2


testLet =
  let fact = \n -> if n == 0 then 1 else n * fact(n-1)
  in fact(10)

testLet3 = let x = x + 1 in x

testLet2 =
  let x = y + 1
      y = 99
  in x * y


testID = id(id)   
-- returns id

testP = let k = 2 in
  let double = \n -> k * n in
    let k = 9 in
      double k

testE5 = let k = 2 in
  let double = \n -> k * n in
    let k = 9 in
      double k

testE6 = let add = \a -> (\b -> b + a) in (add 3) 2

testE7 = let m = 2 in
  let proc = \n -> m + n
      part = \(g,n) -> \m -> n * g(m)
  in let inc = part(proc, 3) in
      inc 7

main = return ()
