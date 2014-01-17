module FunctionalEnvironment where
import Data.Maybe
import IntBool

emptyEnvF :: EnvF
emptyEnvF = \var -> Nothing

bindF :: String -> a -> (String -> Maybe a) -> (String -> Maybe a)
-- bindF :: String -> Value -> EnvF -> EnvF

bindF var val env = \testVar -> if testVar == var
                                then Just val
                                else env testVar

type EnvF = String -> Maybe Value

-- Evaluate an expression in a (functional) environment
evaluateF :: Exp -> EnvF -> Value
evaluateF (Literal v) env      = v
evaluateF (Unary op a) env     = unary op (evaluateF a env)
evaluateF (Binary op a b) env  = binary op (evaluateF a env) (evaluateF b env)
evaluateF (Variable x) env     = fromJust (env x)        -- changed
evaluateF (Let x exp body) env = evaluateF body newEnv
  where newEnv = bindF x (evaluateF exp env) env             -- changed
