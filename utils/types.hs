module Utils.Types where
import Data.Word (Word32)
import Data.ByteString (ByteString)

newtype ID = ID { idUnwrap :: Word32 }
newtype Priority = Priority { prioUnwrap :: Word32 }
newtype Seconds = Seconds { secUnwrap :: Word32 }
newtype TTR = TTR { ttrUnwrap :: Word32 }
newtype Job = Job { jobUnwrap :: ByteString }
newtype Tube = Tube { tubeUnwrap :: String }

type Delay = Seconds
type Count = Word32
type Amount = Word32

data GenericResponse
	= OK
	| NotFound
