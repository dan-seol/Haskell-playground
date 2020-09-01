import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S

byteStringOne :: L.ByteString
byteStringOne = L.pack [99,97,110]

byteStringTwo :: L.ByteString
byteStringTwo = L.pack [98..120]

byteStringThree :: L.ByteString
byteStringThree = L.pack [98,111,114,116]

threeUnpacked = L.unpack byteStringThree

chunks = L.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48] ]
