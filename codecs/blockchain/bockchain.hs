import           Data.ByteString.UTF8   (fromString, toString)
import qualified Crypto.Hash.SHA256     as SHA256
import qualified Data.ByteString.Base16 as Base16


data Transaction = Transaction { from   :: String   -- Who's paying
                               , to     :: String   -- Who's receiving
                               , amount :: Float    -- How much are they paying
                               } deriving (Generic, Show)

data Block = Block { index    :: Int           -- Index of the block
                   , txs      :: [Transaction] -- List of transactions in the block
                   , hash     :: String        -- Hash of the block
                   , prevHash :: String        -- Prev Hash of the block
                   , nonce    :: Maybe Int     -- Nonce of the block (proof of work)
                   } deriving (Generic, Show)

genesisBlock :: Block
genesisBlock = Block blockIndex blockTxs blockHash prevHash Nothing
    where blockIndex = 0
        blockTxs = [Transaction "heaven" "kendrick" 15]
        blockHash = ""
            prevHash = "000000000000000000000000000000000"

blockAddTx :: Block -> Transaction -> Block
blockAddTx (Block i ts h p n) t = Block i (ts ++ [t]) h p n

hashBlock :: Block -> String
hashBlock (Block blockIndex blockTxs blockHash prevHash _) = toString $ Base16.encode digest
    where blockTxStr = foldr ((++) . show) "" blockTxs
          ctx = SHA256.updates SHA256.init $ fmap fromString [blockTxStr, prevHash]
          digest = SHA256.finalize ctx

mineBlock :: Block -> Int -> Block
mineBlock b@(Block i t _ p _) n = case head pow of
                                    '0' -> Block i t blockHash p (Just n)
                                    _   -> mineBlock b (n + 1)
    where blockHash = hashBlock b
          ctx = SHA256.updates SHA256.init (fmap fromString [blockHash, show n, p])
          pow = toString . Base16.encode $ SHA256.finalize ctx -- proof of work
