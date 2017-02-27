import Data.Word (Word16)
import Data.Char (ord)
import Data.Bits ((.&.), shiftR)
import Test.HUnit (assertEqual)

-- encodeChar :: Char -> [Word16]
-- encodeChar x
--   | w < 0x10000 = [fromIntegral w]
--   | otherwise   = [fromIntegral a, fromIntegral b]
--   where w = ord x
--         a = ((w - 0x10000) `shiftR` 10) + 0xD800
--         b = (w .&. 0x3FF) + 0xDC00

encodeChar :: Char -> [Word16]
encodeChar x 
  | w < 0x10000 = [fromIntegral w]
  | otherwise = [fromIntegral a, fromIntegral b]
  where w = ord x
        a = ((w - 0x10000) `shiftR` 10) + 0xD800
        b = (w .&. 0x3FF) + 0xDC00

-- testASCII =
--     assertEqual "ASCII encodes as one code unit"
--     1 (length (encodeChar 'a'))

badTest = do
  assertEqual "sestertius encodes as one code unit"
    1 (length (encodeChar '\x10198'))


testOne char = do
    assertEqual "ASCII encodes as one code unit"
        1 (length (encodeChar char))

testASCII = mapM_ testOne ['\0'..'\127']