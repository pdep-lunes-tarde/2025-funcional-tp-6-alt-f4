module Library where
import PdePreludat
data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa 20 [Pan, Carne, Cheddar, Pan]

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
descuento porcentajeDescuento unaHamburguesa = unaHamburguesa {precioBase = precioBase unaHamburguesa * (1 - (porcentajeDescuento/100))}

pdepBurger :: Hamburguesa
pdepBurger = (descuento 20 . agregarIngrediente Cheddar . agregarIngrediente Panceta . agrandar . agrandar) cuartoDeLibra
