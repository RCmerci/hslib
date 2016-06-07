module HsLib.Base64.Base64
    (encode
    ,decode
    ) where
 
import Control.Monad (join)
import Data.ByteString as BS hiding(reverse)
import Data.Word
import Data.Char
import Data.Bits
     
base64AlphaBet :: Word8 -> Word8
base64AlphaBet 0 = 65 -- fromIntegral $ ord 'A'
base64AlphaBet 1 = 66 -- fromIntegral $ ord 'B'
base64AlphaBet 2 = 67 -- fromIntegral $ ord 'C'
base64AlphaBet 3 = 68 -- fromIntegral $ ord 'D'
base64AlphaBet 4 = 69 -- fromIntegral $ ord 'E'
base64AlphaBet 5 = 70 -- fromIntegral $ ord 'F'
base64AlphaBet 6 = 71 -- fromIntegral $ ord 'G'
base64AlphaBet 7 = 72 -- fromIntegral $ ord 'H'
base64AlphaBet 8 = 73 -- fromIntegral $ ord 'I'
base64AlphaBet 9 = 74 -- fromIntegral $ ord 'J'
base64AlphaBet 10= 75 -- fromIntegral $ ord 'K' 
base64AlphaBet 11= 76 -- fromIntegral $ ord 'L' 
base64AlphaBet 12= 77 -- fromIntegral $ ord 'M' 
base64AlphaBet 13= 78 -- fromIntegral $ ord 'N' 
base64AlphaBet 14= 79 -- fromIntegral $ ord 'O' 
base64AlphaBet 15= 80 -- fromIntegral $ ord 'P' 
base64AlphaBet 16= 81 -- fromIntegral $ ord 'Q' 
base64AlphaBet 17= 82 -- fromIntegral $ ord 'R' 
base64AlphaBet 18= 83 -- fromIntegral $ ord 'S' 
base64AlphaBet 19= 84 -- fromIntegral $ ord 'T' 
base64AlphaBet 20= 85 -- fromIntegral $ ord 'U' 
base64AlphaBet 21= 86 -- fromIntegral $ ord 'V' 
base64AlphaBet 22= 87 -- fromIntegral $ ord 'W' 
base64AlphaBet 23= 88 -- fromIntegral $ ord 'X' 
base64AlphaBet 24= 89 -- fromIntegral $ ord 'Y' 
base64AlphaBet 25= 90 -- fromIntegral $ ord 'Z' 
base64AlphaBet 26= 97-- fromIntegral $ ord 'a' 
base64AlphaBet 27= 98-- fromIntegral $ ord 'b' 
base64AlphaBet 28= 99-- fromIntegral $ ord 'c' 
base64AlphaBet 29= 100-- fromIntegral $ ord 'd' 
base64AlphaBet 30= 101-- fromIntegral $ ord 'e' 
base64AlphaBet 31= 102-- fromIntegral $ ord 'f' 
base64AlphaBet 32= 103-- fromIntegral $ ord 'g' 
base64AlphaBet 33= 104-- fromIntegral $ ord 'h' 
base64AlphaBet 34= 105-- fromIntegral $ ord 'i' 
base64AlphaBet 35= 106-- fromIntegral $ ord 'j' 
base64AlphaBet 36= 107-- fromIntegral $ ord 'k' 
base64AlphaBet 37= 108-- fromIntegral $ ord 'l' 
base64AlphaBet 38= 109-- fromIntegral $ ord 'm' 
base64AlphaBet 39= 110-- fromIntegral $ ord 'n' 
base64AlphaBet 40= 111-- fromIntegral $ ord 'o' 
base64AlphaBet 41= 112-- fromIntegral $ ord 'p' 
base64AlphaBet 42= 113-- fromIntegral $ ord 'q' 
base64AlphaBet 43= 114-- fromIntegral $ ord 'r' 
base64AlphaBet 44= 115-- fromIntegral $ ord 's' 
base64AlphaBet 45= 116-- fromIntegral $ ord 't' 
base64AlphaBet 46= 117-- fromIntegral $ ord 'u' 
base64AlphaBet 47= 118-- fromIntegral $ ord 'v' 
base64AlphaBet 48= 119-- fromIntegral $ ord 'w' 
base64AlphaBet 49= 120-- fromIntegral $ ord 'x' 
base64AlphaBet 50= 121-- fromIntegral $ ord 'y' 
base64AlphaBet 51= 122-- fromIntegral $ ord 'z' 
base64AlphaBet 52= 48-- fromIntegral $ ord '0' 
base64AlphaBet 53= 49-- fromIntegral $ ord '1' 
base64AlphaBet 54= 50-- fromIntegral $ ord '2' 
base64AlphaBet 55= 51-- fromIntegral $ ord '3' 
base64AlphaBet 56= 52-- fromIntegral $ ord '4' 
base64AlphaBet 57= 53-- fromIntegral $ ord '5' 
base64AlphaBet 58= 54-- fromIntegral $ ord '6' 
base64AlphaBet 59= 55-- fromIntegral $ ord '7' 
base64AlphaBet 60= 56-- fromIntegral $ ord '8' 
base64AlphaBet 61= 57-- fromIntegral $ ord '9' 
base64AlphaBet 62= 43--fromIntegral $ ord '+' 
base64AlphaBet 63= 47--fromIntegral $ ord '/'
base64AlphaBet n = error $ show n

revTable :: Word8 -> Word8                   
revTable 65 = 0  
revTable 66 = 1  
revTable 67 = 2  
revTable 68 = 3  
revTable 69 = 4  
revTable 70 = 5  
revTable 71 = 6  
revTable 72 = 7  
revTable 73 = 8  
revTable 74 = 9  
revTable 75 = 10 
revTable 76 = 11 
revTable 77 = 12 
revTable 78 = 13 
revTable 79 = 14 
revTable 80 = 15 
revTable 81 = 16 
revTable 82 = 17 
revTable 83 = 18 
revTable 84 = 19 
revTable 85 = 20 
revTable 86 = 21 
revTable 87 = 22 
revTable 88 = 23 
revTable 89 = 24 
revTable 90 = 25 
revTable 97 = 26 
revTable 98 = 27 
revTable 99 = 28 
revTable 100 = 29 
revTable 101 = 30 
revTable 102 = 31 
revTable 103 = 32 
revTable 104 = 33 
revTable 105 = 34 
revTable 106 = 35 
revTable 107 = 36 
revTable 108 = 37 
revTable 109 = 38 
revTable 110 = 39 
revTable 111 = 40 
revTable 112 = 41 
revTable 113 = 42 
revTable 114 = 43 
revTable 115 = 44 
revTable 116 = 45 
revTable 117 = 46 
revTable 118 = 47 
revTable 119 = 48 
revTable 120 = 49 
revTable 121 = 50 
revTable 122 = 51 
revTable 48 = 52 
revTable 49 = 53 
revTable 50 = 54 
revTable 51 = 55 
revTable 52 = 56 
revTable 53 = 57 
revTable 54 = 58 
revTable 55 = 59 
revTable 56 = 60 
revTable 57 = 61 
revTable 43 = 62 
revTable 47 = 63 
revTable 61 = 61                -- pad 

              
pad :: Word8
pad = 61 -- fromIntegral $ ord '='

encode :: ByteString -> ByteString
encode s = let ws = BS.unpack s in
           BS.pack $ encode' ws []
    where
      encode' ws r = case get24bits ws of
                       (r', []) -> join $ reverse (r':r)
                       (r', next) -> encode' next (r':r)
      get24bits :: [Word8] -> ([Word8], [Word8])
      get24bits [] = (,) [] []
      get24bits [a] = (,) (consume8Bits a) $ []
      get24bits [a, b] = (,) (consume16Bits a b) $ []
      get24bits [a, b, c] = (,) (consume24Bits a b c) $ []
      get24bits (a:b:c:t) = (,) (consume24Bits a b c) $ t
      consume24Bits a b c = fmap base64AlphaBet
                            [shiftR a 2
                            ,(shiftL a 4 .|. shiftR b 4) .&. 63
                            ,(shiftL b 2 .|. shiftR c 6) .&. 63
                            ,(c .&. 63)]
      consume16Bits a b = fmap base64AlphaBet
                          [shiftR a 2
                          ,(shiftL a 4 .|. shiftR b 4) .&. 63
                          ,(shiftL b 2 .&. 63)] ++ [pad]
      consume8Bits a = fmap base64AlphaBet [shiftR a 2, shiftL a 4 .&. 63] ++ [pad, pad]


-- If non-alphabet characters are ignored, instead of causing rejection
-- of the entire encoding (as recommended), a covert channel that can be
-- used to "leak" information is made possible.  
decode :: ByteString -> ByteString
decode b64 = let ws = BS.unpack b64 in
             BS.pack $ decode' ws []
    where
      decode' ws r = case get4chars ws of
                       (r', []) -> join $ reverse (r':r)
                       (r', next) -> decode' next (r':r)
      get4chars :: [Word8] -> ([Word8], [Word8])
      get4chars [] = ([], [])
      get4chars (a:b:c:d:tail)
                | c == pad && d == pad = (,) [shiftL a' 2 .|. shiftR b' 4] []        -- 61 => '='
                | d == pad = (,) [shiftL a' 2 .|. shiftR b' 4
                                ,shiftL b' 4 .|. shiftR c' 2] []
                | True = (,) [shiftL a' 2 .|. shiftR b' 4
                             ,shiftL b' 4 .|. shiftR c' 2
                             ,shiftL c' 6 .|. d'] tail
          where a' = revTable a
                b' = revTable b
                c' = revTable c
                d' = revTable d
