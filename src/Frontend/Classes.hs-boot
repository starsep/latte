module Classes where

import AbsLatte
import Env

addTypedFnDefClass :: ClassesData -> Ident -> TypedFnDefs -> TypedFnDefs
isCompatibleType :: Type -> Type -> TCMonad Bool
