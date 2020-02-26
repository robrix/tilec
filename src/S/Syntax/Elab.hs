{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module S.Syntax.Elab
( ElabC(..)
) where

import Data.Maybe (fromMaybe)
import S.Syntax
import S.Syntax.Classes

newtype ElabC t = ElabC { runElabC :: Spine t -> t ::: t }

instance (Var Int t, Err t) => Var Int (ElabC t) where
  var n = ElabC $ \ ctx ->
    var n ::: fromMaybe (err ("free variable: " <> show n)) (ctx !? n)

instance (Let Int t, Prob Int t, Type Int t, Err t) => Let Int (ElabC t) where
  let' (tm ::: ty) b = ElabC $ \ ctx ->
    let ty' = elab ctx ty === type'
        tm' = elab ctx tm === ty'
    in let' (tm' ::: ty') (elab (ctx :> term_ ty') . b)

instance (Prob Int t, Type Int t, Err t) => Type Int (ElabC t) where
  type' = ElabC (const type')
  pi' t b = ElabC $ \ ctx ->
    let t' = elab ctx t === type'
    in pi' t' (elab (ctx :> term_ t') . b)

elab :: Spine t -> ElabC t -> t ::: t
elab = flip runElabC
