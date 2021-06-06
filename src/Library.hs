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

-- Funciones auxiliares
modificarVelocidadTiroPorObstaculo :: (Number -> Number) -> TiroDespuesDeObstaculo -> TiroDespuesDeObstaculo
modificarVelocidadTiroPorObstaculo modificacion (UnTiroDespuesDeObstaculo exito tiro) = UnTiroDespuesDeObstaculo exito tiro{velocidad = modificacion (velocidad tiro)}

modificarPrecisionTiroPorObstaculo :: (Number -> Number) -> TiroDespuesDeObstaculo -> TiroDespuesDeObstaculo
modificarPrecisionTiroPorObstaculo modificacion (UnTiroDespuesDeObstaculo exito tiro) = UnTiroDespuesDeObstaculo exito tiro{precision = modificacion (precision tiro)}

modificarAlturaTiroPorObstaculo :: (Number -> Number) -> TiroDespuesDeObstaculo -> TiroDespuesDeObstaculo
modificarAlturaTiroPorObstaculo modificacion (UnTiroDespuesDeObstaculo exito tiro) = UnTiroDespuesDeObstaculo exito tiro{altura = modificacion (altura tiro)}


data TiroDespuesDeObstaculo = UnTiroDespuesDeObstaculo {supero :: Bool, tiroQueSale :: Tiro} deriving (Eq, Show)

tiroDetenido = UnTiro 0 0 0

tiroExitosoDetenido = UnTiroDespuesDeObstaculo {supero = True, tiroQueSale = tiroDetenido}

tiroFallido = UnTiroDespuesDeObstaculo {supero = False, tiroQueSale = tiroDetenido}

type Obstaculo = Tiro -> TiroDespuesDeObstaculo
type CondicionDeSuperacion = Tiro -> Bool
type EfectoObstaculo = TiroDespuesDeObstaculo -> TiroDespuesDeObstaculo

superarObstaculo :: CondicionDeSuperacion -> Tiro -> TiroDespuesDeObstaculo
superarObstaculo condicion tiro = UnTiroDespuesDeObstaculo (condicion tiro) tiro

generarEfectoObstaculo :: EfectoObstaculo -> TiroDespuesDeObstaculo -> TiroDespuesDeObstaculo
generarEfectoObstaculo efectoObstaculo (UnTiroDespuesDeObstaculo False _) = tiroFallido
generarEfectoObstaculo efectoObstaculo tiroDespuesDeObstaculo = efectoObstaculo tiroDespuesDeObstaculo

tiroAlRas tiro = altura tiro == 0

tunelConRampita :: Obstaculo
tunelConRampita = efectoTunelConRampita.superarObstaculo condicionTunelConRampita

condicionTunelConRampita :: CondicionDeSuperacion
condicionTunelConRampita tiro = (precision tiro > 90 && tiroAlRas tiro)

efectoTunelConRampita :: EfectoObstaculo
efectoTunelConRampita = generarEfectoObstaculo (modificarVelocidadTiroPorObstaculo (*2).modificarPrecisionTiroPorObstaculo (const 100).modificarAlturaTiroPorObstaculo (const 0))

laguna :: Number -> Obstaculo
laguna largo = efectoLaguna largo.superarObstaculo condicionLaguna

condicionLaguna :: CondicionDeSuperacion
condicionLaguna tiro = (velocidad tiro > 80 && altura tiro > 1 && altura tiro < 5)

efectoLaguna :: Number -> EfectoObstaculo
efectoLaguna largo = generarEfectoObstaculo (modificarAlturaTiroPorObstaculo (/largo))

hoyo :: Obstaculo
hoyo = efectoHoyo.superarObstaculo condicionHoyo

condicionHoyo :: CondicionDeSuperacion
condicionHoyo tiro = velocidad tiro > 5 && velocidad tiro < 20 && tiroAlRas tiro && precision tiro > 95

efectoHoyo :: EfectoObstaculo
efectoHoyo = generarEfectoObstaculo (const tiroExitosoDetenido)

-- Punto 4 a
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles persona obstaculo = filter (supero.obstaculo.flip golpe persona) palos

-- Punto 4 b

-- Punto 4 c
{-
paloMasUtil :: Jugador ->  [Obstaculo] -> Palo
paloMasUtil persona obstaculos = maximoSegun (cuantosObstaculosConsecutivos obstaculos.flip golpe persona) palos
-}

-- Punto 5
resultadoDePrueba = [(bart, 15), (rafa, 20), (todd, 10)]

jugador = fst
puntos = snd

padresQuePierdenLaApuesta :: [(Jugador, Puntos)] -> [String]
padresQuePierdenLaApuesta jugadores = map (padre.jugador) (niniosQueNoGanaron jugadores)

niniosQueNoGanaron :: [(Jugador, Puntos)] -> [(Jugador, Puntos)]
niniosQueNoGanaron jugadores = filter ((< puntosNinioQueGano jugadores).puntos) jugadores

puntosNinioQueGano :: [(Jugador, Puntos)] -> Puntos
puntosNinioQueGano jugadores = puntos (maximoSegun puntos jugadores)




