module Lib where
import Text.Show.Functions

laVerdad = True


data Gimnasta = UnGimnasta {
    edad :: Float,
    peso :: Float,
    tonificacion :: Float
}deriving (Show)

pancho :: Gimnasta
pancho = UnGimnasta {
    edad = 40,
    peso = 120,
    tonificacion = 1
}

andres :: Gimnasta
andres = UnGimnasta {
    edad = 22,
    peso = 80,
    tonificacion = 6
}

relax :: Float -> Gimnasta -> Gimnasta
relax minutos persona = persona

-- Punto 1)

estaObeso :: Gimnasta -> Bool
estaObeso gimnasta = peso gimnasta > 100

estaSaludable :: Gimnasta -> Bool
estaSaludable gimnasta = (not.estaObeso) gimnasta && tonificacion gimnasta > 5



