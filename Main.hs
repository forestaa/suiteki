module Main where

import Data.Char
{- import Debug.Trace -}
import Data.Binary
import Data.List (foldl')
import Numeric
import System.Environment
import System.Posix.Files
import qualified Data.ByteString.Lazy as B
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

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2 then
    putStrLn "usage: $ /path/to/shizuku (assembly.S) (outputPath)"
  else
    writeBinary args

writeBinary :: [String] -> IO ()
writeBinary args = do
  ss <- readFile (args !! 0)
  let ls = map words $ lines ss
  let parsed = constructByteString $ translateInstructions $ parse ls 0 M.empty
  B.writeFile (args !! 1) parsed
  setFileMode (args !! 1) permission
    where
      permission = ownerModes
                   `unionFileModes` groupReadMode
                   `unionFileModes` groupExecuteMode
                   `unionFileModes` otherReadMode
                   `unionFileModes` otherExecuteMode

constructByteString :: [String] -> B.ByteString
constructByteString [] = B.empty
constructByteString (s:ss) = convertBinaryStringToBinary s `B.append` constructByteString ss

convertBinaryStringToBinary :: String -> B.ByteString
convertBinaryStringToBinary bstr = encode $ (fromIntegral $ toDec bstr :: Word32)

translateInstructions :: [Instruction] -> [String]
translateInstructions [] = []
translateInstructions (x:xs) = instructionToBinaryString x : translateInstructions xs

instructionToBinaryString :: Instruction -> String
instructionToBinaryString (R (opcode, rs, rt, rd, shamt, funct)) = opcode ++ rs ++ rt ++ rd ++ shamt ++ funct
instructionToBinaryString (I (opcode, rs, rt, addressOrImmediate)) = opcode ++ rs ++ rt ++ addressOrImmediate
instructionToBinaryString (J (opcode, address)) = opcode ++ address
instructionToBinaryString (FPC (opcode, fmt, ft, fs, cc, zero, a, fc, cond)) = opcode ++ fmt ++ ft ++ fs ++ cc ++ zero ++ a ++ a ++ fc ++ cond
instructionToBinaryString (FPB (cop1, bc, cc, nd, td, offset)) = cop1 ++ bc ++ cc ++ nd ++ td ++ offset
instructionToBinaryString (FPM (cop1, mt, rt, fs, zero)) = cop1 ++ mt ++ rt ++ fs ++ zero

parse :: [[String]] -> Int -> Environment -> [Instruction]
parse [] _ _ = []
parse (l:ls) instAddr e
  | l == []            = parse ls instAddr e
  | l !! 0 !! 0 == '#' = parse ls instAddr e
  {- | trace ("instAddr: " ++ show instAddr ++ " " ++ show l ++ "\n" ++ show e) False = undefined -}
  | isLabel l          = parse ls instAddr (extendEnv e (l !! 0) (instAddr + 1))
  | l !! 0 == "li"     = parseInstruction l instAddr e ++ parse ls (instAddr + 2) e
  | otherwise          = parseInstruction l instAddr e ++ parse ls (instAddr + 1) e


parseInstruction :: [String] -> Int -> Environment -> [Instruction]
parseInstruction i instAddr e
  | i !! 0 == "add"     = [ R ("000000", addr (i !! 2), addr (i !! 3), addr (i !! 1), "00000", "100000") ]
  | i !! 0 == "sub"     = [ R ("000000", addr (i !! 2), addr (i !! 3), addr (i !! 1), "00000", "100010") ]
  | i !! 0 == "slt"     = [ R ("000000", addr (i !! 2), addr (i !! 3), addr (i !! 1), "00000", "101010") ]
  | i !! 0 == "and"     = [ R ("000000", addr (i !! 2), addr (i !! 3), addr (i !! 1), "00000", "100100") ]
  | i !! 0 == "or"      = [ R ("000000", addr (i !! 2), addr (i !! 3), addr (i !! 1), "00000", "100101") ]
  | i !! 0 == "xor"     = [ R ("000000", addr (i !! 2), addr (i !! 3), addr (i !! 1), "00000", "100110") ]
  | i !! 0 == "jalr"    = [ R ("000000", addr (i !! 2), "00000", addr (i !! 1), "00000", "001001") ] -- hint?
  | i !! 0 == "sll"     = [ R ("000000", "00000", addr (i !! 1), addr (i !! 2), binaryExp (read (i !! 3)) 5, "000000") ]
  | i !! 0 == "srl"     = [ R ("000000", "00000", addr (i !! 1), addr (i !! 2), binaryExp (read (i !! 3)) 5, "000010") ]
  | i !! 0 == "add.s"   = [ R ("010001", "10000", addr (i !! 3), addr (i !! 2), addr (i !! 1), "000000") ]
  | i !! 0 == "sub.s"   = [ R ("010001", "10000", addr (i !! 3), addr (i !! 2), addr (i !! 1), "000001") ]
  | i !! 0 == "mul.s"   = [ R ("010001", "10000", addr (i !! 3), addr (i !! 2), addr (i !! 1), "000010") ]
  | i !! 0 == "div.s"   = [ R ("010001", "10000", addr (i !! 3), addr (i !! 2), addr (i !! 1), "000011") ]
  | i !! 0 == "mov.s"   = [ R ("010001", "10000", "00000", addr (i !! 2), addr (i !! 1), "000110") ]
  | i !! 0 == "addi"    = [ I ("001000", addr (i !! 2), addr (i !! 1), binaryExp (read (i !! 3)) 16) ]
  | i !! 0 == "addiu"   = [ I ("001001", addr (i !! 2), addr (i !! 1), binaryExp (read (i !! 3)) 16) ]
  | i !! 0 == "beq"     = [ I ("000100", addr (i !! 1), addr (i !! 2), labelToAddr (i !! 3) instAddr e) ]
  | i !! 0 == "bne"     = [ I ("000101", addr (i !! 1), addr (i !! 2), labelToAddr (i !! 3) instAddr e) ]
  | i !! 0 == "blez"    = [ I ("000110", addr (i !! 1), "00000",       labelToAddr (i !! 2) instAddr e) ]
  | i !! 0 == "bgez"    = [ I ("000001", addr (i !! 1), "00001",       labelToAddr (i !! 2) instAddr e) ]
  | i !! 0 == "bgtz"    = [ I ("000111", addr (i !! 1), "00000",       labelToAddr (i !! 2) instAddr e) ]
  | i !! 0 == "bltz"    = [ I ("000001", addr (i !! 1), "00000",       labelToAddr (i !! 2) instAddr e) ]
  | i !! 0 == "lui"     = [ I ("001111", "00000",       addr (i !! 1), binaryExp (read (i !! 2)) 16) ]
  | i !! 0 == "ori"     = [ I ("001101", addr (i !! 2), addr (i !! 1), binaryExp (read (i !! 3)) 16) ]
  | i !! 0 == "lw"      = [ parseIndexedInstruction i ]
  | i !! 0 == "sw"      = [ parseIndexedInstruction i ]
  | i !! 0 == "lwc1"    = [ parseIndexedInstruction i ]
  | i !! 0 == "swc1"    = [ parseIndexedInstruction i ]
  | i !! 0 == "jal"     = [ J ("000011", binaryExp (read (i !! 1)) 26) ]
  | i !! 0 == "c.olt.s" = [ FPC ("010001", "10000", addrF (i !! 3), addrF (i !! 2), addrCC (i !! 1), "0", "0", "11", "0100") ]
  | i !! 0 == "c.eq.s"  = [ FPC ("010001", "10000", addrF (i !! 3), addrF (i !! 2), addrCC (i !! 1), "0", "0", "11", "0010") ]
  | i !! 0 == "c.ole.s" = [ FPC ("010001", "10000", addrF (i !! 3), addrF (i !! 2), addrCC (i !! 1), "0", "0", "11", "0110") ]
  | i !! 0 == "bc1t"    = [ FPB ("010001", "01000", addrCC (i !! 1), "0", "1", binaryExp (read (i !! 2)) 16) ]
  | i !! 0 == "mfc1"    = [ FPM ("010001", "00000", addr (i !! 1), addrF (i !! 2), "0000000000") ]
  | i !! 0 == "mtc1"    = [ FPM ("010001", "00100", addr (i !! 1), addrF (i !! 2), "0000000000") ]
  | i !! 0 == "li"      = expandLI i instAddr e
  | i !! 0 == "move"    = expandMOVE i instAddr e
  | otherwise           = []


-- lw $t0, 4($gp) -> I (opcode, $gp, $rt, <4 in binary>)
parseIndexedInstruction :: [String] -> Instruction
parseIndexedInstruction i -- = I (opCode, base, rt, offset)
  | i !! 0 == "lw"   = I ("100011", base, addr (i !! 1), offset)
  | i !! 0 == "sw"   = I ("101011", base, addr (i !! 1), offset)
  | i !! 0 == "lwc1" = I ("110001", base, addr (i !! 1), offset)
  | i !! 0 == "swc1" = I ("111001", base, addr (i !! 1), offset)
  | otherwise = undefined
  where (baseRegName, immediateInDigit) = parseRegisterWithOffset (i !! 2)
        offset = binaryExp (read immediateInDigit) 16
        base = addr baseRegName

-- "3(%r3)" -> ("%r3", "3")
parseRegisterWithOffset :: String -> (String, String)
parseRegisterWithOffset str = (regName, offset)
  where offset = takeWhile (\c -> c /= '(') str
        regName = takeWhile (\c -> c /= ')') $ drop 1 $ dropWhile (\c -> c /= '(') str

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
expandMOVE i instAddr e = parseInstruction ["or", i !! 1, i !! 2, "r0"] instAddr e

removeCommaIfAny :: String -> String
removeCommaIfAny regStr = filter (\c -> c /= ',') regStr

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
isLabel i = length i == 1 && drop (length (i !! 0) - 1) (i !! 0) == ":"

extendEnv :: Environment -> String -> Int -> Environment
extendEnv e label i = M.insert (take (length label - 1) label) i e

-- ".label" e -> "0101010110...01"
labelToAddr :: String -> Int -> Environment -> String
labelToAddr label currentLine e = addrDiff
  where lineNum = case M.lookup label e of
                    Just num -> num
                    Nothing -> undefined
        addrDiff = binaryExp (lineNum - currentLine) 16

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
