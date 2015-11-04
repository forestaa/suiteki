module Main where

import Data.Char
import Data.Maybe
import Debug.Trace
import Data.Binary
import Data.Binary.Put
import Data.List
import Numeric
import System.Environment
import System.Posix.Files
import Options.Applicative
import qualified Data.ByteString.Lazy as B
import qualified Data.Graph as G
import qualified Data.Map as M

type Instruction = [String]

type Environment = M.Map String Int

data Args = Args { assembly :: String
                 , output :: String
                 , library :: String
                 } deriving Show

args :: Parser Args
args = Args
    <$> argument str
        ( metavar "INPUT"
       <> help "Input file for suiteki" )
    <*> strOption
        ( long "output"
       <> short 'o'
       <> help "The location of the output file" )
    <*> strOption
        ( long "lib"
       <> short 'l'
       <> metavar "LIBRARY"
       <> help "The location of libmincaml.S" )

main :: IO()
main = execParser opts >>= writeBinary
  where
    opts = info (helper <*> args)
      ( fullDesc
     <> progDesc "Assemble the given file"
     <> header "suiteki - The super-cool assembler for out 1st architecture" )

writeBinary :: Args -> IO ()
writeBinary args = do
    ss <- readFile (assembly args)
    let is = map words $ lines ss -- instructions
    let labels = prepareLabels (map words $ lines ss) 0

    lib <- readFile (library args)
    let ys = map words $ lines lib

    let is' = enrichInstructions (externalFunctions is labels) is ys
    let labels' = prepareLabels is' 0

    let parsed = constructByteString . toString $ parse is' 0 labels'
    B.writeFile (output args) parsed
    setFileMode (output args) permission
  where
    permission = ownerModes
                 `unionFileModes` groupReadMode
                 `unionFileModes` groupExecuteMode
                 `unionFileModes` otherReadMode
                 `unionFileModes` otherExecuteMode

-- read library and add instructions recursively until there is no external
-- function.
enrichInstructions :: [String] -> [[String]] -> [[String]] -> [[String]]
enrichInstructions [] is _ = is ++ [["magic"]]
enrichInstructions (e:es) is lib = enrichInstructions (ext ++ es) is' lib
  where
    is'    = is ++ extractCodeBlock e lib
    labels = prepareLabels is' 0
    ext    = externalFunctions is' labels

constructByteString :: [String] -> B.ByteString
constructByteString = foldr (B.append . convertBinaryStringToBinary) B.empty

convertBinaryStringToBinary :: String -> B.ByteString
convertBinaryStringToBinary bstr = runPut $ putWord32le w
  where
    w = fromIntegral $ toDec bstr :: Word32

toString :: [Instruction] -> [String]
toString = map instructionToBinaryString

instructionToBinaryString :: Instruction -> String
instructionToBinaryString = foldl (\acc x -> acc ++ x) ""

prepareLabels :: [[String]] -> Int -> Environment
prepareLabels [] _ = M.empty
prepareLabels (l:ls) pc
    | null l               = prepareLabels ls pc
    | head (head l) == '#' = prepareLabels ls pc
    | isLabel l            = extendEnv (prepareLabels ls (pc + 1)) (head l) (pc + 1)
    | head l == "li"       = prepareLabels ls (pc + 2)
    | otherwise            = prepareLabels ls (pc + 1)

-- collect all the labels appears in the given assembly.
allLabels :: [[String]] -> [String]
allLabels [] = []
allLabels (l:ls)
    | null l = allLabels ls
    | isLabel l = (take (length (head l) - 1) (head l)) : allLabels ls
    | head l == "beq" = (l !! 3) : allLabels ls
    | head l == "bne" = (l !! 3) : allLabels ls
    | head l == "blez" = (l !! 2) : allLabels ls
    | head l == "bgez" = (l !! 2) : allLabels ls
    | head l == "bgtz" = (l !! 2) : allLabels ls
    | head l == "bltz" = (l !! 2) : allLabels ls
    | head l == "jal" = (l !! 1) : allLabels ls
    | otherwise = allLabels ls

-- collect funtions which is not defined in the given assembly.
externalFunctions :: [[String]] -> Environment -> [String]
externalFunctions instructions e = (nub $ allLabels instructions) \\ locals
  where locals = map (\(k, a) -> k) $ M.toList e

parse :: [[String]] -> Int -> Environment -> [Instruction]
parse [] _ _ = []
parse (l:ls) pc e
    | null l               = parse ls pc e
    | head (head l) == '#' = parse ls pc e
    | isLabel l            = parse ls pc e
    | head l == "li"       = parseInstruction l pc e ++ parse ls (pc + 2) e
    | otherwise            = parseInstruction l pc e ++ parse ls (pc + 1) e

parseInstruction :: [String] -> Int -> Environment -> [Instruction]
parseInstruction i pc e
    | head i == "add"     = [ [ "000000"
                              , addr (i !! 2)
                              , addr (i !! 3)
                              , addr (i !! 1)
                              , "00000"
                              , "100000"
                              ]
                            ]
    | head i == "sub"     = [ [ "000000"
                              , addr (i !! 2)
                              , addr (i !! 3)
                              , addr (i !! 1)
                              , "00000"
                              , "100010"
                              ]
                            ]
    | head i == "slt"     = [ [ "000000"
                              , addr (i !! 2)
                              , addr (i !! 3)
                              , addr (i !! 1)
                              , "00000"
                              , "101010"
                              ]
                            ]
    | head i == "and"     = [ [ "000000"
                              , addr (i !! 2)
                              , addr (i !! 3)
                              , addr (i !! 1)
                              , "00000"
                              , "100100"
                              ]
                            ]
    | head i == "or"      = [ [ "000000"
                              , addr (i !! 2)
                              , addr (i !! 3)
                              , addr (i !! 1)
                              , "00000"
                              , "100101"
                              ]
                            ]
    | head i == "xor"     = [ [ "000000"
                              , addr (i !! 2)
                              , addr (i !! 3)
                              , addr (i !! 1)
                              , "00000"
                              , "100110"
                              ]
                            ]
    | head i == "jalr"    = [ [ "000000"
                              , addr (i !! 2)
                              , "00000"
                              , addr (i !! 1)
                              , "00000"
                              , "001001"
                              ]
                            ]
    | head i == "sll"     = [ [ "000000"
                              , "00000"
                              , addr (i !! 2)
                              , addr (i !! 1)
                              , binaryExp (read (i !! 3)) 5
                              , "000000" ]
                            ]
    | head i == "srl"     = [ [ "000000"
                              , "00000"
                              , addr (i !! 2)
                              , addr (i !! 1)
                              , binaryExp (read (i !! 3)) 5
                              , "000010" ]
                            ]
    | head i == "add.s"   = [ [ "010001"
                              , "10000"
                              , addr (i !! 3)
                              , addr (i !! 2)
                              , addr (i !! 1)
                              , "000000" ]
                            ]
    | head i == "sub.s"   = [ [ "010001"
                              , "10000"
                              , addr (i !! 3)
                              , addr (i !! 2)
                              , addr (i !! 1)
                              , "000001"
                              ]
                            ]
    | head i == "mul.s"   = [ [ "010001"
                              , "10000"
                              , addr (i !! 3)
                              , addr (i !! 2)
                              , addr (i !! 1)
                              , "000010"
                              ]
                            ]
    | head i == "div.s"   = [ [ "010001"
                              , "10000"
                              , addr (i !! 3)
                              , addr (i !! 2)
                              , addr (i !! 1)
                              , "000011"
                              ]
                            ]
    | head i == "mov.s"   = [ [ "010001"
                              , "10000"
                              , "00000"
                              , addr (i !! 2)
                              , addr (i !! 1)
                              , "000110"
                              ]
                            ]
    | head i == "addi"    = [ [ "001000"
                              , addr (i !! 2)
                              , addr (i !! 1)
                              , binaryExp (read (i !! 3)) 16
                              ]
                            ]
    | head i == "addiu"   = [ [ "001001"
                              , addr (i !! 2)
                              , addr (i !! 1)
                              , binaryExp (read (i !! 3)) 16
                              ]
                            ]
    | head i == "beq"     = [ [ "000100"
                              , addr (i !! 1)
                              , addr (i !! 2)
                              , labelToAddr (i !! 3) pc e 16
                              ]
                            ]
    | head i == "bne"     = [ [ "000101"
                              , addr (i !! 1)
                              , addr (i !! 2)
                              , labelToAddr (i !! 3) pc e 16
                              ]
                            ]
    | head i == "blez"    = [ [ "000110"
                              , addr (i !! 1)
                              , "00000"
                              , labelToAddr (i !! 2) pc e 16
                              ]
                            ]
    | head i == "bgez"    = [ [ "000001"
                              , addr (i !! 1)
                              , "00001"
                              , labelToAddr (i !! 2) pc e 16
                              ]
                            ]
    | head i == "bgtz"    = [ [ "000111"
                              , addr (i !! 1)
                              , "00000"
                              , labelToAddr (i !! 2) pc e 16
                              ]
                            ]
    | head i == "bltz"    = [ [ "000001"
                              , addr (i !! 1)
                              , "00000"
                              , labelToAddr (i !! 2) pc e 16
                              ]
                            ]
    | head i == "lui"     = [ [ "001111"
                              , "00000"
                              , addr (i !! 1)
                              , binaryExp (read (i !! 2)) 16
                              ]
                            ]
    | head i == "ori"     = [ [ "001101"
                              , addr (i !! 2)
                              , addr (i !! 1)
                              , binaryExp (read (i !! 3)) 16
                              ]
                            ]
    | head i == "lw"      = [ parseIndexedInstruction i ]
    | head i == "sw"      = [ parseIndexedInstruction i ]
    | head i == "lwc1"    = [ parseIndexedInstruction i ]
    | head i == "swc1"    = [ parseIndexedInstruction i ]
    | head i == "jal"     = [ [ "000011"
                              , labelToAddr (i !! 1) pc e 26
                              ]
                            ]
    | head i == "jr"      = [ [ "00000"
                              , addr (i !! 1)
                              , "0000000000"
                              , "00000"
                              , "001000"
                              ]
                            ]
    | head i == "c.olt.s" = [ [ "010001"
                              , "10000"
                              , addrF (i !! 3)
                              , addrF (i !! 2)
                              , addrCC (i !! 1)
                              , "0"
                              , "0"
                              , "11"
                              , "0100"
                              ]
                            ]
    | head i == "c.eq.s"  = [ [ "010001"
                              , "10000"
                              , addrF (i !! 3)
                              , addrF (i !! 2)
                              , addrCC (i !! 1)
                              , "0"
                              , "0"
                              , "11"
                              , "0010"
                              ]
                            ]
    | head i == "c.ole.s" = [ [ "010001"
                              , "10000"
                              , addrF (i !! 3)
                              , addrF (i !! 2)
                              , addrCC (i !! 1)
                              , "0"
                              , "0"
                              , "11"
                              , "0110"
                              ]
                            ]
    | head i == "bc1t"    = [ [ "010001"
                              , "01000"
                              , addrCC (i !! 1)
                              , "0"
                              , "1"
                              , binaryExp (read (i !! 2)) 16
                              ]
                            ]
    | head i == "mfc1"    = [ [ "010001"
                              , "00000"
                              , addr (i !! 1)
                              , addrF (i !! 2)
                              , "0000000000"
                              ]
                            ]
    | head i == "mtc1"    = [ [ "010001"
                              , "00100"
                              , addr (i !! 1)
                              , addrF (i !! 2)
                              , "0000000000"
                              ]
                            ]
    | head i == "li"      = expandLI i pc e
    | head i == "move"    = expandMOVE i pc e
    | head i == "syscall" = [ [ "000000"
                              , "00000000000000000000"
                              , "001100"
                              ]
                            ]
    | head i == "magic"   = [ [ "11111111111111111111111111111111" ] ]
    | otherwise           = []

-- lw $t0, 4($gp) -> I (opcode, $gp, $rt, <4 in binary>)
parseIndexedInstruction :: [String] -> Instruction
parseIndexedInstruction i -- = I (opCode, base, rt, offset)
    | head i == "lw"   = [ "100011", base, addr (i !! 1), offset ]
    | head i == "sw"   = [ "101011", base, addr (i !! 1), offset ]
    | head i == "lwc1" = [ "110001", base, addr (i !! 1), offset ]
    | head i == "swc1" = [ "111001", base, addr (i !! 1), offset ]
    | otherwise = undefined
  where
    (baseRegName, immediateInDigit) = parseRegisterWithOffset (i !! 2)
    offset = binaryExp (read immediateInDigit) 16
    base = addr baseRegName

-- "3($r3)" -> ("$r3", "3")
parseRegisterWithOffset :: String -> (String, String)
parseRegisterWithOffset str = (regName, offset)
  where
    offset = takeWhile (/= '(') str
    regName = takeWhile (/= ')') $ drop 1 $ dropWhile (/= '(') str

-- "01010" -> 10, "110" -> 6, "10" -> 2, etc.
toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

expandLI :: [String] -> Int -> Environment -> [Instruction]
expandLI i pc e = instructionLUI ++ instructionORI
  where
    immediate = binaryExp (read (i !! 2)) 32
    upper = show . toDec $ take 16 immediate
    lower = show . toDec $ drop 16 immediate
    lui = [ "lui" , i !! 1 , upper ]
    ori = [ "ori" , i !! 1 , i !! 1 , lower ]
    instructionLUI = parseInstruction lui pc e
    instructionORI = parseInstruction ori pc e

expandMOVE :: [String] -> Int -> Environment -> [Instruction]
expandMOVE i = parseInstruction ["or", i !! 1, i !! 2, "r0"]

removeCommaIfAny :: String -> String
removeCommaIfAny = filter (/= ',')

addr :: String -> String
addr mnemonic = unwrapper $ M.lookup regName registerToAddress
  where
    regName = removeCommaIfAny mnemonic
    unwrapper (Just str) = str
    unwrapper Nothing = "00000"

addrCC :: String -> String
addrCC cc = binaryExp (read cc) 3

addrF :: String -> String
addrF mnemonic = unwrapper $ M.lookup mnemonic registerToAddressFloat
  where
    unwrapper (Just str) = str
    unwrapper Nothing = "00000"

binaryExp :: Int -> Int -> String
binaryExp num len
    | num > 0   = replicate (len - length bp) '0' ++ bp
    | num < 0   = replicate (len - length bn) '1' ++ bn
    | otherwise = replicate len '0'
  where
    bp = showIntAtBase 2 intToDigit num ""
    bn = showIntAtBase 2 intToDigit (2 ^ len + num) ""

isLabel :: [String] -> Bool
isLabel i = length i == 1 && drop (length (head i) - 1) (head i) == ":"

extendEnv :: Environment -> String -> Int -> Environment
extendEnv e label i = M.insert (take (length label - 1) label) i e

-- ".label" e -> "0101010110...01"
labelToAddr :: String -> Int -> Environment -> Int -> String
labelToAddr label currentLine e len = addrDiff
  where
    lineNum = fromMaybe undefined (M.lookup label e)
    addrDiff = binaryExp (lineNum - currentLine) len

-- Extract the code block of given |label|.
-- e.g. if |instructions| is as follows:
--   Label1:
--     instrcution1 reg1, reg2
--     instruction2 reg1, reg2
--     ...
--     instructionN reg1, reg2
--   Label2:
--     ...
-- then the code block of |Label1| is
--   [ "instruction1 reg1, reg2"
--   , "instruction2 reg1, reg2"
--   , ...
--   , "instructionN reg1, reg2"
--   ]
-- This function is used to extract a specific part of libmincaml.S.
extractCodeBlock :: String -> [[String]] -> [[String]]
extractCodeBlock label is = takeWhile p1 $ dropWhile p2 is
  where
    p1 = (\i -> i == [label ++ ":"] || not (isLabel i))
    p2 = (\i -> i /= [label ++ ":"])

registerToAddress :: M.Map String String
registerToAddress = M.fromList [ ("$r0",   "00000")
                               , ("$zero", "00000")
                               , ("$r1",   "00001")
                               , ("$at",   "00001")
                               , ("$r2",   "00010")
                               , ("$v0",   "00010")
                               , ("$r3",   "00011")
                               , ("$v1",   "00011")
                               , ("$r4",   "00100")
                               , ("$a0",   "00100")
                               , ("$r5",   "00101")
                               , ("$a1",   "00101")
                               , ("$r6",   "00110")
                               , ("$a2",   "00110")
                               , ("$r7",   "00111")
                               , ("$a3",   "00110")
                               , ("$r8",   "01000")
                               , ("$t0",   "01000")
                               , ("$r9",   "01001")
                               , ("$t1",   "01001")
                               , ("$r10",  "01010")
                               , ("$t2",   "01010")
                               , ("$r11",  "01011")
                               , ("$t3",   "01011")
                               , ("$r12",  "01100")
                               , ("$t4",   "01100")
                               , ("$r13",  "01101")
                               , ("$t5",   "01101")
                               , ("$r14",  "01110")
                               , ("$t6",   "01110")
                               , ("$r15",  "01111")
                               , ("$t7",   "01111")
                               , ("$r16",  "10000")
                               , ("$s0",   "10000")
                               , ("$r17",  "10001")
                               , ("$s1",   "10001")
                               , ("$r18",  "10010")
                               , ("$s2",   "10010")
                               , ("$r19",  "10011")
                               , ("$s3",   "10011")
                               , ("$r20",  "10100")
                               , ("$s4",   "10100")
                               , ("$r21",  "10101")
                               , ("$s5",   "10101")
                               , ("$r22",  "10110")
                               , ("$s6",   "10110")
                               , ("$r23",  "10111")
                               , ("$s7",   "10111")
                               , ("$r24",  "11000")
                               , ("$t8",   "11000")
                               , ("$r25",  "11001")
                               , ("$t9",   "11001")
                               , ("$r26",  "11010")
                               , ("$k0",   "11010")
                               , ("$r27",  "11011")
                               , ("$k1",   "11011")
                               , ("$r28",  "11100")
                               , ("$gp",   "11100")
                               , ("$r29",  "11101")
                               , ("$sp",   "11101")
                               , ("$r30",  "11110")
                               , ("$s8",   "11110")
                               , ("$hp",   "11110")
                               , ("$r31",  "11111")
                               , ("$ra",   "11111")
                               , ("$cl",   "11111")
                               ]

registerToAddressFloat :: M.Map String String
registerToAddressFloat = M.fromList [ ("$f0",  "00000")
                                    , ("$f1",  "00001")
                                    , ("$fsw", "00001")
                                    , ("$f2",  "00010")
                                    , ("$f3",  "00011")
                                    , ("$f4",  "00100")
                                    , ("$f5",  "00101")
                                    , ("$f6",  "00110")
                                    , ("$f7",  "00111")
                                    , ("$f8",  "01000")
                                    , ("$f9",  "01001")
                                    , ("$f10", "01010")
                                    , ("$f11", "01011")
                                    , ("$f12", "01100")
                                    , ("$f13", "01101")
                                    , ("$f14", "01110")
                                    , ("$f15", "01111")
                                    , ("$f16", "10000")
                                    , ("$f17", "10001")
                                    , ("$f18", "10010")
                                    , ("$f19", "10011")
                                    , ("$f20", "10100")
                                    , ("$f21", "10101")
                                    , ("$f22", "10110")
                                    , ("$f23", "10111")
                                    , ("$f24", "11000")
                                    , ("$f25", "11001")
                                    , ("$f26", "11010")
                                    , ("$f27", "11011")
                                    , ("$f28", "11100")
                                    , ("$f29", "11101")
                                    , ("$f30", "11110")
                                    , ("$f31", "11111")
                                    ]
