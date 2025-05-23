module Library where
import PdePreludat
data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | Papas | PatiVegano | BaconDeTofu | PanIntegral
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas = 10
precioIngrediente PanIntegral = 3
precioIngrediente PatiVegano = 10
precioIngrediente BaconDeTofu = 12

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa 20 [Pan, Carne, Cheddar, Pan]

--1

precioFinal :: Hamburguesa -> Number
precioFinal (Hamburguesa preciobase ingredientes) = preciobase + sumatoriaPrecio ingredientes

sumatoriaPrecio :: [Ingrediente] -> Number
sumatoriaPrecio = sum . map precioIngrediente


agrandar :: Hamburguesa -> Hamburguesa
agrandar unaHamburguesa
    |tieneIngrediente Carne unaHamburguesa = agregarIngrediente Carne unaHamburguesa 
    |tieneIngrediente Pollo unaHamburguesa = agregarIngrediente Pollo unaHamburguesa 
    
tieneIngrediente :: Ingrediente -> Hamburguesa -> Bool
tieneIngrediente unIngrediente unaHamburguesa = any (==unIngrediente) (ingredientes unaHamburguesa)


agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente unIngrediente unaHamburguesa = unaHamburguesa {ingredientes = ingredientes unaHamburguesa ++ [unIngrediente]} 

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentajeDescuento unaHamburguesa = unaHamburguesa {precioBase = aplicarDescuento porcentajeDescuento (precioBase unaHamburguesa)}

aplicarDescuento :: Number -> Number -> Number
aplicarDescuento descuento numero = numero * (1 - (descuento/100))

pdepBurger :: Hamburguesa
pdepBurger = descuento 20 . agregarIngrediente Cheddar . agregarIngrediente Panceta . agrandar . agrandar $ cuartoDeLibra

--2 

dobleCuarto :: Hamburguesa
dobleCuarto = agregarIngrediente Carne . agregarIngrediente Cheddar $ cuartoDeLibra

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente Curry dobleCuarto

delDia :: Hamburguesa -> Hamburguesa
delDia = agregarIngrediente Papas . descuento 30 


-- parte 3

ingredienteVegano :: Ingrediente -> Ingrediente
ingredienteVegano Carne = PatiVegano
ingredienteVegano Pollo = PatiVegano
ingredienteVegano Cheddar = QuesoDeAlmendras
ingredienteVegano Panceta = BaconDeTofu
ingredienteVegano otracosa = otracosa



hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie = actualizarPrecio . cambiarIngredientes

cambiarIngredientes :: Hamburguesa -> Hamburguesa
cambiarIngredientes = modificarIngredientes (map ingredienteVegano)

actualizarPrecio :: Hamburguesa -> Hamburguesa
actualizarPrecio unaHamburguesa = unaHamburguesa {precioBase = precioFinal unaHamburguesa - sumatoriaPrecio (ingredientes unaHamburguesa)}


modificarIngredientes unOunosIngredientes unaHamburguesa = unaHamburguesa {ingredientes= unOunosIngredientes (ingredientes unaHamburguesa)}

cambiarPanDePati unaHamburguesa = unaHamburguesa { ingredientes = map reemplazarPan(ingredientes unaHamburguesa), precioBase = precioBase unaHamburguesa + cantidadPanes unaHamburguesa * diferenciaPrecio}


cantidadPanes unaHamburguesa= length (filter (== Pan) (ingredientes unaHamburguesa))
diferenciaPrecio = precioIngrediente PanIntegral - precioIngrediente Pan

reemplazarPan :: Ingrediente -> Ingrediente
reemplazarPan Pan = PanIntegral
reemplazarPan otracosa = otracosa


dobleCuartoVegano = cambiarPanDePati . hacerVeggie $ dobleCuarto