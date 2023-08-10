module Value where


data Value
  = VInt Int
  | VBool Bool
  | VFun (Value -> Value)
  | VUnit