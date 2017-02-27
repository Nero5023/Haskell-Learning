import EncodeChar
import Test.HUnit (assertEqual)

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