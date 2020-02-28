import Data.Char (ord,chr)
import Data.Word 
import System.Environment
import System.IO
type BFWord = Word8
data BFInstructionType = Increment | Decrement | ShiftL | ShiftR | Write | Read | Loop | Nop deriving(Eq,Show) 
data BFState = Run | Output | Input | End deriving(Eq,Show)
data BFTree = BFNode {
    bfType :: BFInstructionType,
    next  :: BFTree,
    body :: BFTree
} | Empty deriving(Eq,Show)

-- not used, should it?
asList :: BFTree -> [(BFTree,BFTree)]
asList Empty = []
asList tree  = (tree, body tree) : asList (next tree)

data BFVM = BFVM {
    iHead :: BFTree,
    stack :: [BFTree],
    mem :: [BFWord],
    mPtr :: Int,
    state :: BFState
} deriving(Show)

main = do
    args <- getArgs
    if null args then
        putStrLn  "./bf source.b"
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
            putChar.chr.fromIntegral $ mem vm !! mPtr vm
            hFlush stdout
            execBFVM vm{state=Run}
        | state vm == End = return ()

buildVM code = BFVM{iHead=bfParse code, stack=[], mem=[0], mPtr=0, state=Run}

runFrame :: BFVM -> BFVM
runFrame vm 
    |iHead vm == Empty = do 
        let s = stack vm
        if not $ null s then popStack vm --loop end
        else vm{state = End} -- Instructions end
    |otherwise = goNext ({-((trace "\n")(traceShow $ bfType $ iHead vm))(-}
    do
        let i = iHead vm
        let bi = body i
        case bfType i of
            Loop ->
                if mem vm !! mPtr vm  == 0 || bi == Empty then vm
                else (pushStack vm) {iHead= BFNode Nop bi Empty} --eeehhhmmmmm...
            Increment -> alterMem vm (1+)
            Decrement -> alterMem vm (255+)
            ShiftL    -> shiftMem vm 1
            ShiftR    -> shiftMem vm (-1)
            Write     -> vm{ state = Output }
            Read      -> vm{ state = Input }
        )
    --)
goNext :: BFVM -> BFVM
goNext vm = vm {iHead=next $ iHead vm}
pushStack :: BFVM -> BFVM
pushStack vm = vm{stack = stack vm ++[iHead vm]}
popStack :: BFVM -> BFVM
popStack vm = do 
    let stackHead = last (stack vm) 
    let newStack = init (stack vm) 
    vm{iHead = stackHead, stack = newStack }

alterMem :: BFVM -> (BFWord->BFWord) -> BFVM
alterMem vm fn = vm { 
    mem = take (mPtr vm) (mem vm) ++ [ fn.fromIntegral $ mem vm !! mPtr vm ] ++ drop ( 1 + mPtr vm) (mem vm)
}
shiftMem :: BFVM -> Int -> BFVM
shiftMem vm val
    | mPtr vm ==  length (mem vm) - 1 && val>0 = 
        shiftMem vm{mem = mem vm ++ replicate 10 0} val
    | mPtr vm == 0 && val<0 = 
        vm{mem = replicate 10 0 ++ mem vm, mPtr = 9}
    | otherwise = vm{mPtr = val + mPtr vm}
-- for debug
runWhileNotIO :: BFVM -> BFVM
runWhileNotIO vm 
    | state vm == Run = runWhileNotIO $ runFrame vm
    | otherwise = vm
runNFrame 0 vm = vm
runNFrame n vm = runNFrame (n-1) (runFrame vm)

---PARSING
bfParse :: String -> BFTree
bfParse [] = Empty
bfParse (i:str)
    |i `elem` "+-><,." = BFNode (charToIns i) (bfParse str) Empty
    |i == '[' = do
        let (strBody,_:strNexts) =  splitAtLoopEnd str 
        BFNode Loop (bfParse strNexts) (bfParse strBody)
    |otherwise = bfParse str

splitAtLoopEnd code = splitAt (getCorrespodentParethesisIndex code) code

     
getCorrespodentParethesisIndex code = 
    snd.head.filter ((0==).fst) $ scanl acc (1,0)  $ mapPar code
    where 
        acc (a,_) (b,c) = (a+b,c)
        mapPar code = filter ((1==).abs.fst) $ zip parValues [0,1..] 
                where parValues =  map ((92-) . fromIntegral . ord) code -- '['=91 ']'=93

charToIns c = case c of 
    '+'-> Increment
    '-'-> Decrement
    '>'-> ShiftR                  
    '<'-> ShiftL
    '.'-> Write
    ','-> Read 
    _  -> Nop 

