module MicroEntrega1 where
import Text.Show.Functions


--Tipos 
    
	
type Programa = Int -> Int -> NuevoMicroprocesador
type NuevoMicroprocesador = Microprocesador -> Microprocesador


--Data


data Microprocesador = UnMicroprocesador {acumuladorA :: Int, acumuladorB :: Int, programa :: [Programa], pc :: Int, datos :: [Int], ultimoError :: String}  deriving (Show)



--Microprocesadores    
           
		   
xt8088 = UnMicroprocesador {acumuladorA = 0, acumuladorB = 0, programa = [], pc = 0, datos = [], ultimoError = ""}       

at8086 = UnMicroprocesador {acumuladorA = 0, acumuladorB = 0, programa = [], pc = 0, datos = [1..20], ultimoError = ""}  
 
fp20 = UnMicroprocesador {acumuladorA = 7, acumuladorB = 24, programa = [], pc = 0, datos = [], ultimoError = ""} 

microDesorden = UnMicroprocesador { acumuladorA = 0, acumuladorB = 0, programa = [], pc = 0, datos = [2,5,1,0,6,9], ultimoError = ""}      



-- Instrucciones  


nop :: NuevoMicroprocesador 
nop = incrementarPc                                                                 

add :: NuevoMicroprocesador
add = incrementarPc.sumarAcumuladores    

sumarAcumuladores :: NuevoMicroprocesador
sumarAcumuladores unMicroprocesador = unMicroprocesador{acumuladorA = acumuladorA unMicroprocesador + acumuladorB unMicroprocesador, acumuladorB = 0}                                                        

swap :: NuevoMicroprocesador
swap = incrementarPc.intercambiarValores  

intercambiarValores :: NuevoMicroprocesador
intercambiarValores unMicroprocesador = cargarUnValorEnElAcumuladorB (acumuladorA unMicroprocesador) (cargarUnValorEnElAcumuladorA (acumuladorB unMicroprocesador) unMicroprocesador) 

lodv :: Int -> NuevoMicroprocesador
lodv valor = incrementarPc.(cargarUnValorEnElAcumuladorA valor)

cargarUnValorEnElAcumuladorA :: Int -> NuevoMicroprocesador 
cargarUnValorEnElAcumuladorA  valor unMicroprocesador = unMicroprocesador{acumuladorA = valor}

cargarUnValorEnElAcumuladorB :: Int -> NuevoMicroprocesador 
cargarUnValorEnElAcumuladorB  valor unMicroprocesador = unMicroprocesador{acumuladorB = valor}


lod :: Int -> NuevoMicroprocesador
lod posicion unMicroprocesador
   | datos unMicroprocesador == [] = memoriaVacia unMicroprocesador
   | otherwise = (incrementarPc.cargarElContenidoDeDatosEnAcumuladorA posicion) unMicroprocesador

cargarElContenidoDeDatosEnAcumuladorA :: Int -> NuevoMicroprocesador 
cargarElContenidoDeDatosEnAcumuladorA posicion unMicroprocesador =  cargarUnValorEnElAcumuladorA  ((!!) (datos unMicroprocesador) posicion  ) unMicroprocesador

str :: Programa
str posicion valor = incrementarPc.(guardaElValorEnLaPosicion posicion valor)

guardaElValorEnLaPosicion :: Programa
guardaElValorEnLaPosicion posicion valor unMicroprocesador = unMicroprocesador {datos = (primeraParteDeLosDatos (posicion) (datos unMicroprocesador)) ++ (segundaParteDeLosDatos posicion valor (datos unMicroprocesador))}


divide :: NuevoMicroprocesador
divide unMicroprocesador
   | acumuladorB unMicroprocesador == 0 = agregarErrorBy0 unMicroprocesador
   | otherwise = (incrementarPc.dividirAcumuladores) unMicroprocesador

cargarPrograma :: Programa -> NuevoMicroprocesador
cargarPrograma programaAAgregar unMicroprocesador = unMicroprocesador {programa = programaAAgregar : programa unMicroprocesador }

memoriaOrdenada :: Microprocesador -> String
memoriaOrdenada unMicroprocesador = mostrarCadena (listaOrdenada (datos unMicroprocesador))


--Otras funciones


incrementarPc :: NuevoMicroprocesador
incrementarPc unMicroprocesador = unMicroprocesador {pc = pc unMicroprocesador + 1} 

sumarValores :: Int -> NuevoMicroprocesador 
sumarValores  valor = swap.(lodv valor)

dividirAcumuladores :: NuevoMicroprocesador
dividirAcumuladores unMicroprocesador =  unMicroprocesador{acumuladorA = div (acumuladorA unMicroprocesador) (acumuladorB unMicroprocesador), acumuladorB =0 }

antesDeDividir :: Programa
antesDeDividir numerador denominador = (lod 1).swap.(lod 2).(str 2 denominador).(str 1 numerador) 

agregarErrorBy0 :: NuevoMicroprocesador
agregarErrorBy0 unMicroprocesador = unMicroprocesador {ultimoError = "ERROR DIVISION BY ZERO" }

resultadoDeDividirPor0 :: Programa
resultadoDeDividirPor0 numerador denominador = incrementarPc.agregarErrorBy0.(antesDeDividir numerador denominador)

memoriaVacia :: NuevoMicroprocesador
memoriaVacia unMicroprocesador = unMicroprocesador{datos = replicate 0 1024}

primeraParteDeLosDatos :: Int -> [Int] -> [Int]
primeraParteDeLosDatos posicion lista = take (posicion - 1) (lista)

segundaParteDeLosDatos :: Int -> Int -> [Int] -> [Int]
segundaParteDeLosDatos posicion valor lista = valor:(drop (posicion) (lista))

mostrarCadena :: Bool -> String
mostrarCadena False = "Memoria desordenada"
mostrarCadena True = "Memoria Ordenada" 

listaOrdenada :: [Int] -> Bool
listaOrdenada [] = True
listaOrdenada [_] = True
listaOrdenada (x:y:xs) = (x<=y) && listaOrdenada (y:xs)
    

-- Programas      
  
  
avanzarTresPosicionesPc :: NuevoMicroprocesador
avanzarTresPosicionesPc = nop.nop.nop                                                 

programaParaSumar :: Programa 
programaParaSumar  valor1 valor2 = add.(lodv valor1).(sumarValores valor2) 

programaParaDividir :: Programa 
programaParaDividir numerador denominador = divide.(antesDeDividir numerador denominador)
