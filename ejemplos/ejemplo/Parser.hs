{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
module Parser where

import Token
import Expr
#if __GLASGOW_HASKELL__ >= 503
import qualified GHC.Exts as Happy_GHC_Exts
#else
import qualified GlaExts as Happy_GHC_Exts
#endif

-- parser produced by Happy Version 1.18.4

data HappyAbsSyn t4 t5
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5

action_0 (6#) = happyShift action_4
action_0 (9#) = happyShift action_5
action_0 (10#) = happyShift action_6
action_0 (11#) = happyShift action_7
action_0 (15#) = happyShift action_8
action_0 (4#) = happyGoto action_9
action_0 (5#) = happyGoto action_3
action_0 x = happyTcHack x happyFail

action_1 (6#) = happyShift action_4
action_1 (9#) = happyShift action_5
action_1 (10#) = happyShift action_6
action_1 (11#) = happyShift action_7
action_1 (15#) = happyShift action_8
action_1 (4#) = happyGoto action_2
action_1 (5#) = happyGoto action_3
action_1 x = happyTcHack x happyFail

action_2 (6#) = happyShift action_4
action_2 (9#) = happyShift action_5
action_2 (10#) = happyShift action_6
action_2 (11#) = happyShift action_7
action_2 (15#) = happyShift action_8
action_2 (4#) = happyGoto action_2
action_2 (5#) = happyGoto action_10
action_2 x = happyTcHack x happyReduce_3

action_3 (14#) = happyShift action_14
action_3 x = happyTcHack x happyReduce_1

action_4 (10#) = happyShift action_13
action_4 x = happyTcHack x happyFail

action_5 x = happyTcHack x happyReduce_8

action_6 x = happyTcHack x happyReduce_9

action_7 (10#) = happyShift action_12
action_7 x = happyTcHack x happyFail

action_8 (6#) = happyShift action_4
action_8 (9#) = happyShift action_5
action_8 (10#) = happyShift action_6
action_8 (11#) = happyShift action_7
action_8 (15#) = happyShift action_8
action_8 (4#) = happyGoto action_2
action_8 (5#) = happyGoto action_11
action_8 x = happyTcHack x happyFail

action_9 (6#) = happyShift action_4
action_9 (9#) = happyShift action_5
action_9 (10#) = happyShift action_6
action_9 (11#) = happyShift action_7
action_9 (15#) = happyShift action_8
action_9 (17#) = happyAccept
action_9 (4#) = happyGoto action_2
action_9 (5#) = happyGoto action_10
action_9 x = happyTcHack x happyReduce_3

action_10 (6#) = happyReduce_2
action_10 (7#) = happyReduce_2
action_10 (8#) = happyReduce_2
action_10 (9#) = happyReduce_2
action_10 (10#) = happyReduce_2
action_10 (11#) = happyReduce_2
action_10 (14#) = happyShift action_14
action_10 (15#) = happyReduce_2
action_10 (16#) = happyReduce_2
action_10 (17#) = happyReduce_2
action_10 x = happyTcHack x happyReduce_2

action_11 (14#) = happyShift action_14
action_11 (16#) = happyShift action_18
action_11 x = happyTcHack x happyReduce_1

action_12 (12#) = happyShift action_17
action_12 x = happyTcHack x happyFail

action_13 (13#) = happyShift action_16
action_13 x = happyTcHack x happyFail

action_14 (6#) = happyShift action_4
action_14 (9#) = happyShift action_5
action_14 (10#) = happyShift action_6
action_14 (11#) = happyShift action_7
action_14 (15#) = happyShift action_8
action_14 (4#) = happyGoto action_2
action_14 (5#) = happyGoto action_15
action_14 x = happyTcHack x happyFail

action_15 (6#) = happyReduce_6
action_15 (7#) = happyReduce_6
action_15 (8#) = happyReduce_6
action_15 (9#) = happyReduce_6
action_15 (10#) = happyReduce_6
action_15 (11#) = happyReduce_6
action_15 (14#) = happyShift action_14
action_15 (15#) = happyReduce_6
action_15 (16#) = happyReduce_6
action_15 (17#) = happyReduce_6
action_15 x = happyTcHack x happyReduce_6

action_16 (6#) = happyShift action_4
action_16 (9#) = happyShift action_5
action_16 (10#) = happyShift action_6
action_16 (11#) = happyShift action_7
action_16 (15#) = happyShift action_8
action_16 (4#) = happyGoto action_2
action_16 (5#) = happyGoto action_20
action_16 x = happyTcHack x happyFail

action_17 (6#) = happyShift action_4
action_17 (9#) = happyShift action_5
action_17 (10#) = happyShift action_6
action_17 (11#) = happyShift action_7
action_17 (15#) = happyShift action_8
action_17 (4#) = happyGoto action_2
action_17 (5#) = happyGoto action_19
action_17 x = happyTcHack x happyFail

action_18 x = happyTcHack x happyReduce_7

action_19 (6#) = happyReduce_5
action_19 (7#) = happyReduce_5
action_19 (8#) = happyReduce_5
action_19 (9#) = happyReduce_5
action_19 (10#) = happyReduce_5
action_19 (11#) = happyReduce_5
action_19 (14#) = happyShift action_14
action_19 (15#) = happyReduce_5
action_19 (16#) = happyReduce_5
action_19 (17#) = happyReduce_5
action_19 x = happyTcHack x happyReduce_5

action_20 (7#) = happyShift action_21
action_20 (14#) = happyShift action_14
action_20 x = happyTcHack x happyReduce_1

action_21 (6#) = happyShift action_4
action_21 (9#) = happyShift action_5
action_21 (10#) = happyShift action_6
action_21 (11#) = happyShift action_7
action_21 (15#) = happyShift action_8
action_21 (4#) = happyGoto action_2
action_21 (5#) = happyGoto action_22
action_21 x = happyTcHack x happyFail

action_22 (8#) = happyShift action_23
action_22 (14#) = happyShift action_14
action_22 x = happyTcHack x happyReduce_1

action_23 x = happyTcHack x happyReduce_4

happyReduce_1 = happySpecReduce_1  4# happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4# happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (App happy_var_1 happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5# happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happyReduce 7# 5# happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (App (Abs happy_var_2 happy_var_6) happy_var_4
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 4# 5# happyReduction_5
happyReduction_5 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Abs happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_3  5# happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_3)
	(HappyTerminal (TokenOp happy_var_2))
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Op (opEnc happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  5# happyReduction_7
happyReduction_7 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  5# happyReduction_8
happyReduction_8 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn5
		 (Num happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  5# happyReduction_9
happyReduction_9 (HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn5
		 (Var happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 17# 17# notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenLet -> cont 6#;
	TokenIn -> cont 7#;
	TokenEnd -> cont 8#;
	TokenInt happy_dollar_dollar -> cont 9#;
	TokenSym happy_dollar_dollar -> cont 10#;
	TokenLambda -> cont 11#;
	TokenArrow -> cont 12#;
	TokenEq -> cont 13#;
	TokenOp happy_dollar_dollar -> cont 14#;
	TokenLParen -> cont 15#;
	TokenRParen -> cont 16#;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

expr tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


opEnc :: String -> Binop
opEnc "+" = Add
opEnc "-" = Sub
opEnc "*" = Mul

parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Expr
parseExpr = expr . scanTokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates/GenericTemplate.hs" #-}








{-# LINE 49 "templates/GenericTemplate.hs" #-}

{-# LINE 59 "templates/GenericTemplate.hs" #-}

{-# LINE 68 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 1#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 1# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j ) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Happy_GHC_Exts.Int# ->                    -- token number
         Happy_GHC_Exts.Int# ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 1# tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop 0# l = l
happyDrop n ((_):(t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 253 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (1# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  1# tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  1# tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action 1# 1# tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action 1# 1# tk (HappyState (action)) sts ( (HappyErrorToken (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 317 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
