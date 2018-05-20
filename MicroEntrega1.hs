module MicroEntrega1 where
import Text.Show.Functions


--Tipos 
    

type Programa = Microprocesador -> Microprocesador
type NuevoMicroprocesador = Microprocesador -> Microprocesador


--Data


data Microprocesador = UnMicroprocesador {acumuladorA :: Int, acumuladorB :: Int, programa :: Programa, pc :: Int, datos :: [Int], ultimoError :: String} deriving (Show)



--Microprocesadores    
           

xt8088 = UnMicroprocesador {acumuladorA = 0, acumuladorB = 0, programa = programaVacio, pc = 0, datos = [], ultimoError = ""}       

at8086 = UnMicroprocesador {acumuladorA = 0, acumuladorB = 0, programa = programaVacio, pc = 0, datos = [1..20], ultimoError = ""}  
 
fp20 = UnMicroprocesador {acumuladorA = 7, acumuladorB = 24, programa = programaVacio, pc = 0, datos = [], ultimoError = ""} 

microDesorden = UnMicroprocesador {acumuladorA = 0, acumuladorB = 0, programa = programaVacio, pc = 0, datos = [2,5,1,0,6,9], ultimoError = ""} 

microConMemoriaInfinita = UnMicroprocesador {acumuladorA = 0, acumuladorB = 0, programa = programaVacio, pc = 0, datos = [0,0..], ultimoError = ""} 


--Instrucciones  


nop :: NuevoMicroprocesador 
nop unMicroprocesador
   | errorEnElMicro unMicroprocesador = unMicroprocesador
   | otherwise = incrementarPc unMicroprocesador                                                                 

add :: NuevoMicroprocesador
add unMicroprocesador
   | errorEnElMicro unMicroprocesador = unMicroprocesador
   | otherwise = (incrementarPc.sumarAcumuladores) unMicroprocesador    

sumarAcumuladores :: NuevoMicroprocesador
sumarAcumuladores unMicroprocesador = unMicroprocesador {acumuladorA = acumuladorA unMicroprocesador + acumuladorB unMicroprocesador, acumuladorB = 0}                                                        

swap :: NuevoMicroprocesador
swap unMicroprocesador
   | errorEnElMicro unMicroprocesador = unMicroprocesador
   | otherwise                        = (incrementarPc.intercambiarValores) unMicroprocesador  
   
intercambiarValores :: NuevoMicroprocesador
intercambiarValores unMicroprocesador = ((cargarUnValorEnElAcumuladorB.accessorAcumuladorA)  unMicroprocesador) (cargarUnValorEnElAcumuladorA (acumuladorB unMicroprocesador) unMicroprocesador) 

lodv :: Int -> NuevoMicroprocesador
lodv valor unMicroprocesador
   | errorEnElMicro unMicroprocesador = unMicroprocesador
   | otherwise                        = (incrementarPc.(cargarUnValorEnElAcumuladorA valor)) unMicroprocesador

cargarUnValorEnElAcumuladorA :: Int -> NuevoMicroprocesador 
cargarUnValorEnElAcumuladorA  valor unMicroprocesador = unMicroprocesador {acumuladorA = valor}

cargarUnValorEnElAcumuladorB :: Int -> NuevoMicroprocesador 
cargarUnValorEnElAcumuladorB  valor unMicroprocesador = unMicroprocesador {acumuladorB = valor}


lod :: Int -> NuevoMicroprocesador
lod posicion unMicroprocesador
   | errorEnElMicro unMicroprocesador = unMicroprocesador
   | datos unMicroprocesador == []    = memoriaVacia unMicroprocesador
   | otherwise                        = (incrementarPc.cargarElContenidoDeDatosEnAcumuladorA posicion) unMicroprocesador

cargarElContenidoDeDatosEnAcumuladorA :: Int -> NuevoMicroprocesador 
cargarElContenidoDeDatosEnAcumuladorA posicion unMicroprocesador =  cargarUnValorEnElAcumuladorA ((!!) (datos unMicroprocesador) (posicion - 1)) unMicroprocesador

str :: Int -> Int -> NuevoMicroprocesador
str posicion valor unMicroprocesador
   | errorEnElMicro unMicroprocesador = unMicroprocesador
   | otherwise                        = (incrementarPc.(guardaElValorEnLaPosicion posicion valor)) unMicroprocesador

guardaElValorEnLaPosicion :: Int -> Int -> NuevoMicroprocesador
guardaElValorEnLaPosicion posicion valor unMicroprocesador = unMicroprocesador {datos = (primeraParteDeLosDatos (posicion) (datos unMicroprocesador)) ++ (segundaParteDeLosDatos posicion valor (datos unMicroprocesador))}


divide :: NuevoMicroprocesador
divide unMicroprocesador
   | errorEnElMicro unMicroprocesador   = unMicroprocesador
   | acumuladorB unMicroprocesador == 0 = (incrementarPc.agregarErrorBy0) unMicroprocesador
   | otherwise                          = (incrementarPc.dividirAcumuladores) unMicroprocesador

cargarPrograma :: Programa -> NuevoMicroprocesador
cargarPrograma programaAAgregar unMicroprocesador = unMicroprocesador {programa = programaAAgregar}

ejecutarPrograma :: NuevoMicroprocesador
ejecutarPrograma unMicroprocesador = (programa unMicroprocesador) unMicroprocesador

ifnz :: NuevoMicroprocesador -> NuevoMicroprocesador
ifnz conjuntoDeInstrucciones unMicroprocesador
   | acumuladorA unMicroprocesador == 0 = unMicroprocesador
   | otherwise                          = conjuntoDeInstrucciones unMicroprocesador

memoriaOrdenada :: Microprocesador -> String
memoriaOrdenada = mostrarComentario.listaOrdenada.accessorDatos

accessorDatos :: Microprocesador -> [Int]
accessorDatos (UnMicroprocesador _ _ _ _ datos _) = datos 

accessorAcumuladorA (UnMicroprocesador acumuladorA _ _ _  _ _) = acumuladorA 

--Otras funciones


incrementarPc :: NuevoMicroprocesador
incrementarPc unMicroprocesador = unMicroprocesador {pc = pc unMicroprocesador + 1} 

sumarValores :: Int -> NuevoMicroprocesador 
sumarValores  valor = swap.(lodv valor)

dividirAcumuladores :: NuevoMicroprocesador
dividirAcumuladores unMicroprocesador =  unMicroprocesador {acumuladorA = div (acumuladorA unMicroprocesador) (acumuladorB unMicroprocesador), acumuladorB =0}

antesDeDividir :: Int -> Int -> NuevoMicroprocesador
antesDeDividir numerador denominador = (lod 1).swap.(lod 2).(str 2 denominador).(str 1 numerador) 

agregarErrorBy0 :: NuevoMicroprocesador
agregarErrorBy0 unMicroprocesador = unMicroprocesador {ultimoError = "ERROR DIVISION BY ZERO"}

resultadoDeDividirPor0 :: Int -> Int -> NuevoMicroprocesador
resultadoDeDividirPor0 numerador denominador = incrementarPc.agregarErrorBy0.(antesDeDividir numerador denominador)

memoriaVacia :: NuevoMicroprocesador
memoriaVacia unMicroprocesador = unMicroprocesador {datos = replicate 0 1024}

primeraParteDeLosDatos :: Int -> [Int] -> [Int]
primeraParteDeLosDatos posicion lista = take (posicion - 1) (lista)

segundaParteDeLosDatos :: Int -> Int -> [Int] -> [Int]
segundaParteDeLosDatos posicion valor lista = valor : (drop (posicion) (lista))

errorEnElMicro :: Microprocesador -> Bool
errorEnElMicro unMicroprocesador = ultimoError unMicroprocesador /= ""

programaVacio :: Programa
programaVacio unMicroprocesador = unMicroprocesador

mostrarComentario :: Bool -> String
mostrarComentario False = "Memoria desordenada"
mostrarComentario True  = "Memoria Ordenada" 

listaOrdenada :: [Int] -> Bool
listaOrdenada [] = True
listaOrdenada [_] = True
listaOrdenada (primerElemento : segundoElemento : cola) = (primerElemento <= segundoElemento) && listaOrdenada (segundoElemento : cola)
 

--Programas      
  
  
avanzarTresPosicionesPc :: NuevoMicroprocesador
avanzarTresPosicionesPc = nop.nop.nop                                                 

programaParaSumar :: Int -> Int -> NuevoMicroprocesador 
programaParaSumar  valor1 valor2 = add.(lodv valor1).(sumarValores valor2) 

programaParaDividir :: Int -> Int -> NuevoMicroprocesador 
programaParaDividir numerador denominador = divide.(antesDeDividir numerador denominador)


--Pruebas

-- ejecutarPrograma (cargarPrograma (programaParaSumar 10 22) xt8088)

-- ejecutarPrograma (cargarPrograma (programaParaDividir 2 0) xt8088)

-- lod 2 (divide (lodv 2 xt8088))

-- divide (lod 2 (divide (nop xt8088)))

-- ifnz ((lod 1).swap.(lod 2).(str 2 3).(str 1 2)) fp20

-- ifnz (swap.lodv 3) fp20
