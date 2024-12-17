module Library where
import PdePreludat
import Data.Int (Int)

doble :: Number -> Number
doble numero = numero + numero

data Personaje = UnPersonaje {
    nombre :: String,
    puntaje :: Number,
    inventario:: [Material]
} deriving (Show)

type Material = String

data Receta = UnaReceta{
    nombreReceta :: String,
    matNecesarios :: [Material],
    tiempoDeConstruccion :: Number,
    resultado :: Material
} deriving (Show)

fogata, polloAsado, sueter, madera, fosforo, pollo, lana, agujas, tintura :: Material
fogata = "fogata"
polloAsado = "polloAsado"
sueter = "sueter"
madera = "madera"
fosforo = "fosforo"
pollo = "pollo"
lana = "lana"
agujas = "agujas"
tintura = "tintura"

recetaFogata :: Receta
recetaFogata = UnaReceta{nombreReceta="fogata", matNecesarios = [madera, fosforo], tiempoDeConstruccion = 10, resultado=fogata}

recetaPolloAsado :: Receta
recetaPolloAsado = UnaReceta{nombreReceta="polloAsado", matNecesarios = [fogata, pollo], tiempoDeConstruccion = 300, resultado=polloAsado}

recetaSueter :: Receta
recetaSueter = UnaReceta{nombreReceta="sueter", matNecesarios = [lana, agujas, tintura], tiempoDeConstruccion = 600, resultado=sueter}

craftear :: Receta -> Personaje -> Personaje
craftear receta jugador  
        |cuentaConMaterialesRequeridos jugador receta  = (agregarObj(resultado receta) . quitarMatsUsados(matNecesarios receta) . alterarPuntos(tiempoDeConstruccion receta))jugador
        |otherwise = jugador{puntaje = puntaje jugador - 100}

cuentaConMaterialesRequeridos :: Personaje -> Receta -> Bool
cuentaConMaterialesRequeridos jugador receta = all (contieneElemDeReceta jugador) (matNecesarios receta)  

contieneElemDeReceta :: Personaje  -> Material -> Bool
contieneElemDeReceta personaje material = material `elem` inventario personaje

alterarPuntos :: Number -> Personaje -> Personaje
alterarPuntos n personaje = personaje{puntaje = puntaje personaje + 10*n}

quitarMatsUsados :: [Material] -> Personaje -> Personaje
quitarMatsUsados materiales personaje = personaje{inventario = foldr quitarUnaVez (inventario personaje) materiales  }

quitarUnaVez :: Material -> [Material] -> [Material]
quitarUnaVez _ [] = []
quitarUnaVez mat (m:ms)
    |mat == m = ms
    |otherwise = m:quitarUnaVez mat ms

agregarObj :: Material -> Personaje -> Personaje
agregarObj mat personaje = personaje{inventario =  mat:inventario personaje}


{- funciona pero no tiene sentido
listarMat :: [Material] -> Material -> [Material]
listarMat [] mat = [mat]
listarMat [materiales] mat = mat:[materiales]
-}

--2 

objQueDuplicanPuntaje :: Personaje -> [Receta] -> [Material]
objQueDuplicanPuntaje personaje = map resultado . filter (recetaDuplicaPuntaje personaje) 

recetaDuplicaPuntaje :: Personaje -> Receta -> Bool
recetaDuplicaPuntaje personaje receta = 2 * puntaje personaje <= 10 * tiempoDeConstruccion receta

craftearSucesivamente :: Personaje -> [Receta] -> Personaje
craftearSucesivamente = foldr craftear  

quedaConMismosPuntos :: Personaje -> [Receta] -> Bool
quedaConMismosPuntos personaje recetas = puntaje (craftearSucesivamente personaje recetas) == puntaje (craftearSucesivamente personaje (reverse recetas))

--Mine

data Bioma = UnBioma{
    materiales :: [Material],
    elementoNecesario :: Material
} deriving (Show)


type Herramienta = [Material] -> Material

hacha :: Herramienta
hacha = last 

espada :: Herramienta
espada = head

pico :: Number -> Herramienta
pico = flip (!!)


minar :: Herramienta -> Bioma -> Personaje -> Personaje
minar herramienta bioma personaje 
    |contieneElemDeReceta personaje (elementoNecesario bioma) = (agregarObj (herramienta(materiales bioma))  . alterarPuntos 5) personaje
    |otherwise = personaje 


--2

materialDelMedio :: Herramienta
materialDelMedio materiales = pico (length materiales `div` 2) materiales


--materialConMasLetras = pico (\materiales -> length materiales `div` 2)

biomaConMatsInfinitos :: Material -> [Material]
biomaConMatsInfinitos mat = mat:biomaConMatsInfinitos mat

biomaInfinito :: Bioma
biomaInfinito = UnBioma (biomaConMatsInfinitos madera) fosforo

uri :: Personaje
uri = UnPersonaje{nombre = "uri", puntaje = 200, inventario = [fosforo, polloAsado, sueter]}

