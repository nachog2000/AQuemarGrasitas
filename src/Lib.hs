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


-- Punto 2)

quemarCalorias :: Gimnasta -> Float -> Gimnasta
quemarCalorias gimnasta calorias | estaObeso gimnasta = gimnasta {peso = bajarPeso gimnasta (calorias/150)} 
                                 | (not.estaObeso) gimnasta && edad gimnasta > 30 && calorias < 200 = gimnasta {peso = bajarPeso gimnasta 1}
                                 | otherwise = gimnasta {peso = bajarPeso gimnasta (calorias / (peso gimnasta * edad gimnasta))}

bajarPeso :: Gimnasta -> Float -> Float
bajarPeso gimnasta cantidad = peso gimnasta - cantidad
