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


-- Punto 3)

type Ejercicio = Minuto -> Gimnasta -> Gimnasta
type Minuto = Float

caminata :: Ejercicio
caminata minutos gimnasta = quemarCalorias gimnasta (minutos * 5)

entrenamientoEnCinta :: Ejercicio
entrenamientoEnCinta minutos gimnasta = quemarCalorias gimnasta (velocidadPromedio minutos * minutos)

velocidadPromedio :: Minuto -> Float
velocidadPromedio tiempo = (velocidadInicial + velocidadFinal tiempo) / 2

velocidadInicial :: Float
velocidadInicial = 6

velocidadFinal :: Minuto->Float
velocidadFinal tiempo = velocidadInicial + (tiempo/5)

pesas :: Float ->  Ejercicio
pesas kilos minutos gimnasta | minutos > 10 = tonificar (kilos / 10) gimnasta
                             | otherwise = gimnasta

tonificar :: Float -> Gimnasta -> Gimnasta
tonificar cantidad gimnasta = gimnasta {tonificacion = tonificacion gimnasta + cantidad}

colina :: Float -> Ejercicio
colina inclinacion minutos gimnasta = quemarCalorias gimnasta (2 * minutos * inclinacion)

montaña :: Float -> Ejercicio
montaña inclinacion minutos = (tonificar 1). (primeraColina inclinacion minutos). (segundaColina inclinacion minutos)

primeraColina :: Float -> Ejercicio
primeraColina inclinacion minutos = colina inclinacion (minutos / 2)

segundaColina :: Float -> Ejercicio
segundaColina inclinacion minutos = colina (inclinacion + 3) (minutos / 2)
