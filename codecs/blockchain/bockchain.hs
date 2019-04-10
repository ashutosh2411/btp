import           Data.ByteString.UTF8   (fromString, toString)
import qualified Crypto.Hash.SHA256     as SHA256
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString as S (ByteString, unpack)
import Data.ByteString.Char8 as C8 (pack)
import Data.Char (chr)



data Transaction = Transaction { from   :: String   -- Who's paying
                               , to     :: String   -- Who's receiving
                               , amount :: Float    -- How much are they paying
                               } deriving (Generic, Show)

data Block = Block { index    :: Int           -- Index of the block
                   , txs      :: [Transaction] -- List of transactions in the block
                   , hash     :: String        -- Hash of the block
                   , prevHash :: String        -- Prev Hash of the block
                   , nonce    :: Maybe Int     -- Nonce of the block (proof of work)
                   } deriving (Generic, Show, Hashable)

instance Hashable Transaction 
instance Hashable Float   

genesisBlock :: Block
genesisBlock = Block blockIndex blockTxs blockHash prevHash Nothing
    where blockIndex = 0
        blockTxs = [Transaction "heaven" "kendrick" 15]
        blockHash = ""
            prevHash = "000000000000000000000000000000000"

bsToStr :: S.ByteString -> String
bsToStr = map (chr . fromEnum) . S.unpack

blockAddTx :: Block -> Transaction -> Block
blockAddTx (Block i ts h p n) t = Block i (ts ++ [t]) h p n

hashBlock :: Block -> String
hashBlock (Block blockIndex blockTxs blockHash prevHash _) = toString $ Base16.encode digest
    where blockTxStr = foldr ((++) . show) "" blockTxs
          ctx = SHA256.updates SHA256.init $ fmap fromString [blockTxStr, prevHash]
          digest = SHA256.finalize ctx

dtHashBlock :: Block -> String
dtHashBlock (Block index tracs blockHash prevHash) = toString $ Base16.encode (computeHash SHA256 tracs)

mineBlock :: Block -> Int -> Block
mineBlock b@(Block i t _ p _) n = case head pow of
                                    '0' -> Block i t blockHash p (Just n)
                                    _   -> mineBlock b (n + 1)
    where blockHash = hashBlock b
          ctx = SHA256.updates SHA256.init (fmap fromString [blockHash, show n, p])
          pow = toString . Base16.encode $ SHA256.finalize ctx -- proof of work

dtMineBlock :: Block -> Int -> Block
dtMineBlock b@(Block i t _ p _) n = case head pow of 
                                      '0' -> Block i t blockHash p (Just n)
                                      _   -> mineBlock b (n + 1)
    where blockHash = dtHashBlock b 
          pow = toString $ Base16.encode (computeHash SHA256 (b,n,p))     -- Proof of work