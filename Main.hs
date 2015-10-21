module Main where

import Data.Char
import Data.Maybe
import Debug.Trace
import Data.Binary
import Data.List (foldl')
import Numeric
import System.Environment
import System.Posix.Files
import Options.Applicative
import qualified Data.ByteString.Lazy as B
import qualified Data.Graph as G
import qualified Data.Map as M

type InstructionR         = (String, String, String, String, String, String)
type InstructionI         = (String, String, String, String)
type InstructionJ         = (String, String)
type InstructionFPCompare = (String, String, String, String, String, String, String, String, String)
type InstructionFPBranch  = (String, String, String, String, String, String)
type InstructionFPMove    = (String, String, String, String, String)

data Instruction = R InstructionR
                 | I InstructionI
                 | J InstructionJ
                 | FPC InstructionFPCompare
                 | FPB InstructionFPBranch
                 | FPM InstructionFPMove

type Environment = M.Map String Int

data Args = Args { assembly :: String, output :: String, library :: String } deriving Show

args :: Parser Args
args = Args
  <$> strOption
      ( long "input"
     <> short 'i'
     <> metavar "INPUT"
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
  let ls = map words $ lines ss
  let labels = prepareLabels ls 0
  let parsed = constructByteString $ translateInstructions $ parse ls 0 labels
  B.writeFile (output args) parsed
  setFileMode (output args) permission
    where
      permission = ownerModes
                   `unionFileModes` groupReadMode
                   `unionFileModes` groupExecuteMode
                   `unionFileModes` otherReadMode
                   `unionFileModes` otherExecuteMode

constructByteString :: [String] -> B.ByteString
constructByteString = foldr (B.append . convertBinaryStringToBinary) B.empty

convertBinaryStringToBinary :: String -> B.ByteString
convertBinaryStringToBinary bstr = encode (fromIntegral $ toDec bstr :: Word32)

translateInstructions :: [Instruction] -> [String]
translateInstructions = map instructionToBinaryString

instructionToBinaryString :: Instruction -> String
instructionToBinaryString (R (opcode, rs, rt, rd, shamt, funct)) = opcode ++ rs ++ rt ++ rd ++ shamt ++ funct
instructionToBinaryString (I (opcode, rs, rt, addressOrImmediate)) = opcode ++ rs ++ rt ++ addressOrImmediate
instructionToBinaryString (J (opcode, address)) = opcode ++ address
instructionToBinaryString (FPC (opcode, fmt, ft, fs, cc, zero, a, fc, cond)) = opcode ++ fmt ++ ft ++ fs ++ cc ++ zero ++ a ++ a ++ fc ++ cond
instructionToBinaryString (FPB (cop1, bc, cc, nd, td, offset)) = cop1 ++ bc ++ cc ++ nd ++ td ++ offset
instructionToBinaryString (FPM (cop1, mt, rt, fs, zero)) = cop1 ++ mt ++ rt ++ fs ++ zero

prepareLabels :: [[String]] -> Int -> Environment
prepareLabels [] _ = M.empty
prepareLabels (l:ls) instAddr
  | trace (show instAddr ++ ": " ++ show l) False = undefined
  | null l               = prepareLabels ls instAddr
  | head (head l) == '#' = prepareLabels ls instAddr
  | isLabel l            = extendEnv (prepareLabels ls (instAddr + 1)) (head l) (instAddr + 1)
  | head l == "li"       = prepareLabels ls (instAddr + 2)
  | otherwise            = prepareLabels ls (instAddr + 1)


parse :: [[String]] -> Int -> Environment -> [Instruction]
parse [] _ _ = []
parse (l:ls) instAddr e
  | null l             = parse ls instAddr e
  | head (head l) == '#' = parse ls instAddr e
  | trace ("instAddr: " ++ show instAddr ++ " " ++ show l ++ "\n" ++ show e) False = undefined
  | isLabel l          = parse ls instAddr e
  | head l == "li"     = parseInstruction l instAddr e ++ parse ls (instAddr + 2) e
  | otherwise          = parseInstruction l instAddr e ++ parse ls (instAddr + 1) e

parseInstruction :: [String] -> Int -> Environment -> [Instruction]
parseInstruction i instAddr e
  | head i == "add"     = [ R ("000000", addr (i !! 2), addr (i !! 3), addr (i !! 1), "00000", "100000") ]
  | head i == "sub"     = [ R ("000000", addr (i !! 2), addr (i !! 3), addr (i !! 1), "00000", "100010") ]
  | head i == "slt"     = [ R ("000000", addr (i !! 2), addr (i !! 3), addr (i !! 1), "00000", "101010") ]
  | head i == "and"     = [ R ("000000", addr (i !! 2), addr (i !! 3), addr (i !! 1), "00000", "100100") ]
  | head i == "or"      = [ R ("000000", addr (i !! 2), addr (i !! 3), addr (i !! 1), "00000", "100101") ]
  | head i == "xor"     = [ R ("000000", addr (i !! 2), addr (i !! 3), addr (i !! 1), "00000", "100110") ]
  | head i == "jalr"    = [ R ("000000", addr (i !! 2), "00000", addr (i !! 1), "00000", "001001") ] -- hint?
  | head i == "sll"     = [ R ("000000", "00000", addr (i !! 2), addr (i !! 1), binaryExp (read (i !! 3)) 5, "000000") ]
  | head i == "srl"     = [ R ("000000", "00000", addr (i !! 2), addr (i !! 1), binaryExp (read (i !! 3)) 5, "000010") ]
  | head i == "add.s"   = [ R ("010001", "10000", addr (i !! 3), addr (i !! 2), addr (i !! 1), "000000") ]
  | head i == "sub.s"   = [ R ("010001", "10000", addr (i !! 3), addr (i !! 2), addr (i !! 1), "000001") ]
  | head i == "mul.s"   = [ R ("010001", "10000", addr (i !! 3), addr (i !! 2), addr (i !! 1), "000010") ]
  | head i == "div.s"   = [ R ("010001", "10000", addr (i !! 3), addr (i !! 2), addr (i !! 1), "000011") ]
  | head i == "mov.s"   = [ R ("010001", "10000", "00000", addr (i !! 2), addr (i !! 1), "000110") ]
  | head i == "addi"    = [ I ("001000", addr (i !! 2), addr (i !! 1), binaryExp (read (i !! 3)) 16) ]
  | head i == "addiu"   = [ I ("001001", addr (i !! 2), addr (i !! 1), binaryExp (read (i !! 3)) 16) ]
  | head i == "beq"     = [ I ("000100", addr (i !! 1), addr (i !! 2), labelToAddr (i !! 3) instAddr e 16) ]
  | head i == "bne"     = [ I ("000101", addr (i !! 1), addr (i !! 2), labelToAddr (i !! 3) instAddr e 16) ]
  | head i == "blez"    = [ I ("000110", addr (i !! 1), "00000",       labelToAddr (i !! 2) instAddr e 16) ]
  | head i == "bgez"    = [ I ("000001", addr (i !! 1), "00001",       labelToAddr (i !! 2) instAddr e 16) ]
  | head i == "bgtz"    = [ I ("000111", addr (i !! 1), "00000",       labelToAddr (i !! 2) instAddr e 16) ]
  | head i == "bltz"    = [ I ("000001", addr (i !! 1), "00000",       labelToAddr (i !! 2) instAddr e 16) ]
  | head i == "lui"     = [ I ("001111", "00000",       addr (i !! 1), binaryExp (read (i !! 2)) 16) ]
  | head i == "ori"     = [ I ("001101", addr (i !! 2), addr (i !! 1), binaryExp (read (i !! 3)) 16) ]
  | head i == "lw"      = [ parseIndexedInstruction i ]
  | head i == "sw"      = [ parseIndexedInstruction i ]
  | head i == "lwc1"    = [ parseIndexedInstruction i ]
  | head i == "swc1"    = [ parseIndexedInstruction i ]
  | head i == "jal"     = [ J ("000011", labelToAddr (i !! 1) instAddr e 26) ]
  | head i == "c.olt.s" = [ FPC ("010001", "10000", addrF (i !! 3), addrF (i !! 2), addrCC (i !! 1), "0", "0", "11", "0100") ]
  | head i == "c.eq.s"  = [ FPC ("010001", "10000", addrF (i !! 3), addrF (i !! 2), addrCC (i !! 1), "0", "0", "11", "0010") ]
  | head i == "c.ole.s" = [ FPC ("010001", "10000", addrF (i !! 3), addrF (i !! 2), addrCC (i !! 1), "0", "0", "11", "0110") ]
  | head i == "bc1t"    = [ FPB ("010001", "01000", addrCC (i !! 1), "0", "1", binaryExp (read (i !! 2)) 16) ]
  | head i == "mfc1"    = [ FPM ("010001", "00000", addr (i !! 1), addrF (i !! 2), "0000000000") ]
  | head i == "mtc1"    = [ FPM ("010001", "00100", addr (i !! 1), addrF (i !! 2), "0000000000") ]
  | head i == "li"      = expandLI i instAddr e
  | head i == "move"    = expandMOVE i instAddr e
  | otherwise           = []

-- lw $t0, 4($gp) -> I (opcode, $gp, $rt, <4 in binary>)
parseIndexedInstruction :: [String] -> Instruction
parseIndexedInstruction i -- = I (opCode, base, rt, offset)
  | head i == "lw"   = I ("100011", base, addr (i !! 1), offset)
  | head i == "sw"   = I ("101011", base, addr (i !! 1), offset)
  | head i == "lwc1" = I ("110001", base, addr (i !! 1), offset)
  | head i == "swc1" = I ("111001", base, addr (i !! 1), offset)
  | otherwise = undefined
  where (baseRegName, immediateInDigit) = parseRegisterWithOffset (i !! 2)
        offset = binaryExp (read immediateInDigit) 16
        base = addr baseRegName

-- "3($r3)" -> ("$r3", "3")
parseRegisterWithOffset :: String -> (String, String)
parseRegisterWithOffset str = (regName, offset)
  where offset = takeWhile (/= '(') str
        regName = takeWhile (/= ')') $ drop 1 $ dropWhile (/= '(') str

-- "01010" -> 10, "110" -> 6, "10" -> 2, etc.
toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

expandLI :: [String] -> Int -> Environment -> [Instruction]
expandLI i instAddr e = instructionLUI ++ instructionORI
  where immediate = binaryExp (read (i !! 2)) 32
        upper = show . toDec $ take 16 immediate -- upper 32 bits represented in decimal
        lower = show . toDec $ drop 16 immediate -- lower 32 bits represented in decimal
        instructionLUI = parseInstruction ["lui", i !! 1, upper] instAddr e
        instructionORI = parseInstruction ["ori", i !! 1, i !! 1, lower] instAddr e

expandMOVE :: [String] -> Int -> Environment -> [Instruction]
expandMOVE i = parseInstruction ["or", i !! 1, i !! 2, "r0"]

removeCommaIfAny :: String -> String
removeCommaIfAny = filter (/= ',')

addr :: String -> String
addr mnemonic = unwrapper $ M.lookup regName registerToAddress
  where regName = removeCommaIfAny mnemonic
        unwrapper (Just str) = str
        unwrapper Nothing = "00000"

addrCC :: String -> String
addrCC cc = binaryExp (read cc) 3

addrF :: String -> String
addrF mnemonic = unwrapper $ M.lookup mnemonic registerToAddressFloat
  where unwrapper (Just str) = str
        unwrapper Nothing = "00000"

binaryExp :: Int -> Int -> String
binaryExp num len
  | num > 0   = replicate (len - length bp) '0' ++ bp
  | num < 0   = replicate (len - length bn) '1' ++ bn
  | otherwise = replicate len '0'
  where bp = showIntAtBase 2 intToDigit num ""
        bn = showIntAtBase 2 intToDigit (2 ^ len + num) ""

isLabel :: [String] -> Bool
isLabel i = length i == 1 && drop (length (head i) - 1) (head i) == ":"

extendEnv :: Environment -> String -> Int -> Environment
extendEnv e label i = M.insert (take (length label - 1) label) i e

-- ".label" e -> "0101010110...01"
labelToAddr :: String -> Int -> Environment -> Int -> String
labelToAddr label currentLine e len = addrDiff
  where lineNum = fromMaybe undefined (M.lookup label e)
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
extractCodeBlock :: String -> [[String]] -> [[String]]
extractCodeBlock label instructions = takeWhile (\i -> not (isLabel i)) $ drop 1 $ dropWhile (\i -> i /= [label ++ ":"]) instructions

registerToAddress :: M.Map String String
registerToAddress = M.fromList [ ("$r0",  "00000"), ("$zero", "00000")
                               , ("$r1",  "00001"), ("$sw",   "00001")
                               , ("$r2",  "00010"), ("$v0",   "00010")
                               , ("$r3",  "00011"), ("$v1",   "00011")
                               , ("$r4",  "00100"), ("$a0",   "00100")
                               , ("$r5",  "00101"), ("$a1",   "00101")
                               , ("$r6",  "00110"), ("$a2",   "00110")
                               , ("$r7",  "00111"), ("$a3",   "00110")
                               , ("$r8",  "01000"), ("$t0",   "01000")
                               , ("$r9",  "01001"), ("$t1",   "01001")
                               , ("$r10", "01010"), ("$t2",   "01010")
                               , ("$r11", "01011"), ("$t3",   "01011")
                               , ("$r12", "01100"), ("$t4",   "01100")
                               , ("$r13", "01101"), ("$t5",   "01101")
                               , ("$r14", "01110"), ("$t6",   "01110")
                               , ("$r15", "01111"), ("$t7",   "01111")
                               , ("$r16", "10000"), ("$s0",   "10000")
                               , ("$r17", "10001"), ("$s1",   "10001")
                               , ("$r18", "10010"), ("$s2",   "10010")
                               , ("$r19", "10011"), ("$s3",   "10011")
                               , ("$r20", "10100"), ("$s4",   "10100")
                               , ("$r21", "10101"), ("$s5",   "10101")
                               , ("$r22", "10110"), ("$s6",   "10110")
                               , ("$r23", "10111"), ("$s7",   "10111")
                               , ("$r24", "11000"), ("$t8",   "11000"), ("$tmp", "11000")
                               , ("$r25", "11001"), ("$t9",   "11001")
                               , ("$r26", "11010"), ("$k0",   "11010")
                               , ("$r27", "11011"), ("$k1",   "11011")
                               , ("$r28", "11100"), ("$gp",   "11100")
                               , ("$r29", "11101"), ("$sp",   "11101")
                               , ("$r30", "11110"), ("$s8",   "11110"), ("$hp", "11110")
                               , ("$r31", "11111"), ("$ra",   "11111"), ("$cl", "11111")
                               ]

registerToAddressFloat :: M.Map String String
registerToAddressFloat = M.fromList [ ("$f0",  "00000")
                                    , ("$f1",  "00001"), ("$fsw", "00001")
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
