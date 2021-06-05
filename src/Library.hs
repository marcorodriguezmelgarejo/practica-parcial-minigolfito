module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

-- Funciones para modificar un tiro
cambiarVelocidadTiro :: (Number -> Number) -> Tiro -> Tiro
cambiarVelocidadTiro comoCambiar tiro = tiro{velocidad = comoCambiar (velocidad tiro)}

cambiarPrecisionTiro :: (Number -> Number) -> Tiro -> Tiro
cambiarPrecisionTiro comoCambiar tiro = tiro{precision = comoCambiar (precision tiro)}

cambiarAlturaTiro :: (Number -> Number) -> Tiro -> Tiro
cambiarAlturaTiro comoCambiar tiro = tiro{altura = comoCambiar (altura tiro)}

-- Tiros de ejemplo
tiroTodo100 = UnTiro 100 100 100

-- Punto 1 a
type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro{velocidad = 10, precision = 2*precisionJugador habilidad, altura = 0}

madera :: Palo
madera habilidad = UnTiro{velocidad = 100, precision = precisionJugador habilidad/2, altura = 5}

hierro :: Number -> Palo
hierro n habilidad = UnTiro{velocidad = fuerzaJugador habilidad * n, precision = precisionJugador habilidad / n, altura = max (n-3) 0}

-- Punto 1 b
palos :: [Palo]
palos = [putter,madera] ++ todosLosHierros

todosLosHierros :: [Palo]
todosLosHierros = map hierro [1 .. 10] 
--esto permite cambiar la cantidad de hierros sin enumerarlos a todos en "palos", solamente cambiando esta lista

-- Punto 2
golpe :: Palo -> Jugador -> Tiro
golpe palo = palo.habilidad
--pongo los parámetros en ese orden para poder definir la función con composición

-- Punto 3 
data TiroDespuesDeObstaculo = UnTiroDespuesDeObstaculo {supero :: Bool, tiroQueSale :: Tiro} deriving (Eq, Show)

tiroDetenido = UnTiro 0 0 0

tiroFallido = UnTiroDespuesDeObstaculo {supero = False, tiroQueSale = tiroDetenido}

type Obstaculo = Tiro -> TiroDespuesDeObstaculo

{-
tunelConRampita :: Obstaculo
tunelConRampita tiro
    | noSuperaTunelConRampita tiro = tiroFallido
    | otherwise = tiro  
-}
