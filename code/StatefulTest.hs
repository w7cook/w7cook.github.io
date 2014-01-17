import Base
import Stateful

mul10 addr mem =
  let IntV n = access addr mem in
    update addr (IntV (10 * n)) mem

testMul10 = mul10 1 [IntV 3, IntV 4, IntV 5, IntV 6]

mul10 :: Int -> Memory -> Memory

main'2 = do
  print testMul10

main = do
  tagged "Upda11" main'2
  
