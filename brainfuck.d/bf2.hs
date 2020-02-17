
import Data.Char (ord,chr)
import Data.Word 
import Debug.Trace
import System.Environment
import System.IO
type BFWord = Word8
data BFInstructionType = Increment | Shift | Write | Read | Loop | Nop deriving(Eq,Show) 
data BFState = Run | Output | Input | End deriving(Eq,Show)
data BFTree = BFNode {
    bfType :: BFInstructionType,
    next  :: BFTree,
    body :: BFTree
}| Operand { opVal::Int }| Empty deriving(Eq,Show)

data BFVM = BFVM {
    iHead :: BFTree,
    stack :: [BFTree],
    mem :: [BFWord],
    mPtr :: Int,
    state :: BFState
} deriving(Show)

main = do
    args <- getArgs
    if length args < 1 then
        putStrLn $ "./bf source.b"
    else do
        code <- readFile $ head args
        execBFVM $ buildVM code


execBFVM :: BFVM -> IO ()
execBFVM vm 
        | state vm == Run = execBFVM $ runFrame vm
        | state vm == Input = do 
            c  <- getChar
            execBFVM $ alterMem vm{state=Run} (\_-> fromIntegral $ ord c)
        | state vm == Output = do
            putChar.chr $ fromIntegral $ (mem vm)!!(mPtr vm)
            hFlush stdout
            execBFVM vm{state=Run}
        | otherwise = return ()

buildVM code = BFVM{iHead=optimize $ bfParse code, stack=[], mem=[0], mPtr=0, state=Run}

runFrame :: BFVM -> BFVM
runFrame vm 
    |iHead vm == Empty = 
        if length (stack vm) > 0 then popStack vm --loop end
        else vm{state = End} -- Program end
    |otherwise = goNext (--((traceShow $ bfType $ iHead vm))(
    do
        let i = iHead vm
        let bi = body i
        case bfType i of
            Loop ->
                if (mem vm)!!(mPtr vm) == 0 || bi == Empty then vm
                else (pushStack vm) {iHead= BFNode Nop bi Empty} --eeehhhmmmmm...
            Increment -> alterMem vm (+(fromIntegral $ opVal bi))
            Shift     -> shiftMem vm (fromIntegral $ opVal bi)
            Write     -> vm{ state = Output }
            Read      -> vm{ state = Input }
            Nop       -> vm
        )
    --)
goNext :: BFVM -> BFVM
goNext vm = vm {iHead=next $ iHead vm}
pushStack :: BFVM -> BFVM
pushStack vm = vm{stack = (stack vm)++[iHead vm]}
popStack :: BFVM -> BFVM
popStack vm = vm{iHead =  last (stack vm) , stack =  init (stack vm) }

alterMem :: BFVM -> (BFWord->BFWord) -> BFVM
alterMem vm fn = vm { 
    mem = (take (mPtr vm) (mem vm)) ++ [fn $ fromIntegral $ ((mem vm)!!(mPtr vm)) ] ++ drop ( 1 + (mPtr vm)) (mem vm)
}
shiftMem :: BFVM -> Int -> BFVM
shiftMem vm val
    | mPtr vm ==  (length (mem vm)) - 1 && val>0 = 
        shiftMem vm{mem = (mem vm)++(replicate (4*val) 0)} val
    | mPtr vm == 0 && val<0 = 
       vm{mem = (replicate (4*(-val)) 0)++(mem vm), mPtr = -val}
    | otherwise = vm{mPtr = val + (mPtr vm)}
-- for debug
runWhileIO :: BFVM -> BFVM
runWhileIO vm 
    | state vm == Run = runWhileIO $ runFrame vm
    | otherwise = vm
runNFrame 0 vm = vm
runNFrame n vm = runNFrame (n-1) (runFrame vm)

---PARSING
bfParse :: String -> BFTree
bfParse [] = Empty
bfParse (i:str)
    |elem i "+>" = BFNode (charToIns i) (bfParse str) (Operand 1)
    |elem i "-<" = BFNode (charToIns i) (bfParse str) (Operand (-1))
    |elem i ",." = BFNode (charToIns i) (bfParse str) Empty
    |i == '[' = do
        let (strBody,_:strNexts) =  splitAtLoopEnd str 
        BFNode Loop (bfParse strNexts) (bfParse strBody)
    |otherwise = bfParse str
    where 
        charToIns c = case c of 
            '+'-> Increment
            '-'-> Increment
            '>'-> Shift                 
            '<'-> Shift
            '.'-> Write
            ','-> Read 
            _  -> Nop


optimize :: BFTree -> BFTree
optimize = dropNop . accumulateNodes
-- | drop instruction with value 0 or loop with empty body
dropNop bfTree 
    |bfTree == Empty = Empty
    |bfType bfTree == Loop =
        if body bfTree == Empty then next bfTree 
        else bfTree{body= dropNop $ body bfTree}--drop from body return this node
    |body bfTree == (Operand 0) = next bfTree 
    |otherwise = bfTree


accumulateNodes tree
            |tree == Empty = Empty
            |next tree == Empty = tree
            |(body $ next tree) == Empty = tree
            |bfType tree == Loop = BFNode Loop (accumulateNodes $ next tree) (accumulateNodes $ body tree)
            |(bfType tree) == (bfType $ next tree) =
            --(traceShow ("--",body tree,body $ next tree))( 
                do
                let n = next tree
                let newOp = (opVal $ body n) + (opVal $ body tree)
                accumulateNodes tree{
                    body = Operand newOp,
                    next = next n 
                } 
                
            --)
            |otherwise = tree{next= accumulateNodes $ next tree}
    


splitAtLoopEnd code = splitAt (getCorrespodentParethesisIndex code) code
mapPar code = filter ((1==).abs.fst) $ zip parValues [0,1..] 
                where parValues =  map ((92-) . fromIntegral . ord) code

     
getCorrespodentParethesisIndex code = 
    snd.head.(filter ((0==).fst)) $ scanl acc (1,0)  $ mapPar code
    where 
        acc = (\(a,_) (b,c) -> (a+b,c) )



prettyTree :: BFTree -> String
prettyTree tree = pretty' tree 0 
    where 
        pretty' tree level = 
            if tree == Empty then  ""
            else
                if bfType tree == Loop then 
                    head ++ (show $ bfType tree) ++['\n'] ++ (pretty' (body tree) (level +1) ) ++ tail
                else if body tree == Empty then
                    head ++ (show $ bfType tree) ++ ['\n'] ++ tail
                else
                    head ++ (show $ bfType tree)++" "++((show.opVal.body) tree) ++['\n'] ++ tail
            where 
                tail =  (pretty' (next tree) level)
                head = replicate level ' '
        

            
