module Library where
import PdePreludat

-- Defino mis Alias
type Nombre = String
type Kilometraje = Number
type Viajes = [Viaje]
type Fecha = String
type Costo = Number
type Barrio = String 
type CondicionViaje = Viaje -> Bool
type Liquidacion = Number
type Choferes = [Chofer]

-- Defino los clientes
data Cliente = Cliente {
    nombreCliente :: Nombre,
    barrio :: Barrio
} deriving Show

-- Defino los viajes
data Viaje = Viaje {
    fecha :: Fecha,
    cliente :: Cliente,
    costo :: Costo
} deriving Show

-- Defino los choferes
data Chofer = Chofer {
    nombre :: Nombre,
    kilometraje :: Kilometraje,
    viajes :: Viajes,
    condicionViaje :: CondicionViaje
} deriving Show

-- Inicializo algunas condiciones
tomaCualquierViaje :: CondicionViaje
tomaCualquierViaje viaje = True

tomaViajeMasDe200 :: CondicionViaje
tomaViajeMasDe200 viaje = costo viaje > 200

tieneMasDe :: Number -> Number -> Bool
tieneMasDe cantidadLetras = (> cantidadLetras) -- Aplicación parcial
tomaViajeNombreClienteLetrasN :: Number -> CondicionViaje
tomaViajeNombreClienteLetrasN cantidadLetras = (tieneMasDe cantidadLetras).(length).(nombreCliente).(cliente) 
-- cliente viaje es una función. La función "cliente" sobre "viaje"

noViveEn :: Barrio -> Barrio -> Bool
noViveEn barrioProhibido = (/= barrioProhibido ) -- barrioProhibido /= barrioCliente
-- Uso aplicación parcial
tomaViajeNoViveEn :: Barrio -> CondicionViaje
tomaViajeNoViveEn barrioNuevo = (noViveEn barrioNuevo).(barrio).(cliente) 
-- La función "cliente" recibe un viaje y devuelve un cliente
-- La función "barrio" recibe un cliente y devuelve un barrio
-- La función "noViveEn" recibe dos barrios y devuelve un booleano

-- Defino las expresiones
lucas = Cliente "Lucas" "Victoria"
daniel = Chofer "Daniel" 23500 [(Viaje "20/04/2017" lucas 150)] (tomaViajeNoViveEn "Olivos")
alejandra = Chofer "Alejandra" 180000 [] tomaCualquierViaje

-- Defino si un chofer puede tomar un viaje
puedeTomarViaje :: Chofer -> Viaje -> Bool
puedeTomarViaje = condicionViaje -- puedeTomarViaje chofer viaje = (condicionViaje chofer) viaje

-- Defino la liquidación de un chofer
hacerLiquidacion :: Viajes -> Liquidacion
hacerLiquidacion = sum.map (costo)

liquidacionChofer :: Chofer -> Liquidacion
liquidacionChofer = hacerLiquidacion.viajes

-- Defino realizar un viaje
agregarViaje :: Viaje -> Chofer -> Chofer
agregarViaje viaje chofer = chofer {viajes = viaje:(viajes chofer)}

tieneMenosViajes :: Chofer -> Chofer -> Chofer
tieneMenosViajes chofer1 chofer2
    | (length(viajes chofer1)) < (length(viajes chofer2)) = chofer1
    | otherwise = chofer2
choferConMenosViajes :: Choferes -> Chofer
choferConMenosViajes choferes = foldl1 (tieneMenosViajes) choferes
-- Cuando tengo que reducir una lista a un elemento uso cualquier foldl
-- En este caso tenia que saber que chofer tenia menos viajes de todos los choferes
-- Entonces con el foldl1 y una funcion que compare de a dos los viajes obtengo el que menos tiene

cumpleCondicion :: Viaje -> Chofer -> Bool
cumpleCondicion viaje chofer = (condicionViaje chofer) viaje
puedenHacerElViaje :: Viaje -> Choferes -> Choferes
puedenHacerElViaje viaje = filter (cumpleCondicion viaje)

realizarViaje :: Viaje -> Choferes -> Chofer
realizarViaje viaje choferes = agregarViaje viaje ((choferConMenosViajes.(puedenHacerElViaje viaje)) choferes)

-- Al infinito y mas allá
viajesInfinitosConLucas :: Viajes
viajesInfinitosConLucas = repeat (Viaje "11/03/201" lucas 50)
nitoInfy = Chofer "Nito Infy" 70000 viajesInfinitosConLucas (tomaViajeNombreClienteLetrasN 3)

-- No se puede calcular la liquidación de Nito ya que al ser infinitos sus viajes, su liquidacion diverge
-- Nito puede tomar el viaje de Lucas de $ 500 el 2/5/2017 ya que Lucas tiene al menos 3 letras

-- Inferir tipo función
-- Ord c => c -> (c -> Bool) -> (a -> c) -> [a] -> c / Pongo en la consola :t gongNeng
gongNeng arg1 arg2 arg3 = max arg1 . head . filter arg2 . map arg3
