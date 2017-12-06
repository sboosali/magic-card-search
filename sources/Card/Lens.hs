module Card.Lens where
import Card.Types
import Control.Lens (makeLenses) 

makeLenses ''CardObject 
