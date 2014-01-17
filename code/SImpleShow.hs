import Simple

instance Show Exp where
  show (Number i)      = show i
  show (Add a b)       = showBinary a "+" b
  show (Subtract a b)  = showBinary a "-" b
  show (Multiply a b)  = showBinary a "*" b
  show (Divide a b)  = showBinary a "/" b
showBinary a op b = show a ++ op ++ show b

