module MicroEntrega1 where


--Data                                                                                                               

data Microprocesador = UnMicroprocesador {acumuladorA :: Int, acumuladorB :: Int, pc :: Int, datos :: [Int], ultimoError :: String }  deriving Show



--Microprocesadores                                                                        
xt8088 = UnMicroprocesador {acumuladorA = 0, acumuladorB = 0, pc = 0, datos = [], ultimoError = ""}       

at8086 = UnMicroprocesador {acumuladorA = 0, acumuladorB = 0, pc = 0, datos = [1..20], ultimoError = ""}  
 
fp20 = UnMicroprocesador {acumuladorA = 7, acumuladorB = 24, pc = 0, datos = [], ultimoError = ""}        



-- Instrucciones  

type NuevoMicroprocesador = Microprocesador -> Microprocesador

nop:: NuevoMicroprocesador 
nop = incrementarPc                                                                 

add:: NuevoMicroprocesador
add = incrementarPc.sumarAcumuladores    

sumarAcumuladores :: NuevoMicroprocesador
sumarAcumuladores unMicroprocesador = unMicroprocesador{acumuladorA = acumuladorA unMicroprocesador + acumuladorB unMicroprocesador, acumuladorB = 0}                                                        

swap:: NuevoMicroprocesador
swap = incrementarPc.intercambiarValores  

intercambiarValores :: NuevoMicroprocesador
intercambiarValores unMicroprocesador = cargarUnValorEnElAcumuladorB (acumuladorA unMicroprocesador) (cargarUnValorEnElAcumuladorA (acumuladorB unMicroprocesador) unMicroprocesador) 

lodv :: Int-> NuevoMicroprocesador
lodv valor = incrementarPc.(cargarUnValorEnElAcumuladorA valor)

cargarUnValorEnElAcumuladorA :: Int->NuevoMicroprocesador 
cargarUnValorEnElAcumuladorA  valor unMicroprocesador = unMicroprocesador{acumuladorA = valor}


cargarUnValorEnElAcumuladorB :: Int->NuevoMicroprocesador 
cargarUnValorEnElAcumuladorB  valor unMicroprocesador = unMicroprocesador{acumuladorB = valor}


lod :: Int->NuevoMicroprocesador
lod posicion unMicroprocesador
   | datos unMicroprocesador == [] = memoriaVacia unMicroprocesador
   | otherwise = (incrementarPc.cargarElContenidoDeDatosEnAcumuladorA posicion) unMicroprocesador


cargarElContenidoDeDatosEnAcumuladorA :: Int -> NuevoMicroprocesador 
cargarElContenidoDeDatosEnAcumuladorA posicion unMicroprocesador =  cargarUnValorEnElAcumuladorA  ((!!) (datos unMicroprocesador) posicion  ) unMicroprocesador

str :: Int->Int->NuevoMicroprocesador
str posicion valor = incrementarPc.(guardaElValorEnLaPosicion posicion valor)

guardaElValorEnLaPosicion :: Int->Int->NuevoMicroprocesador
guardaElValorEnLaPosicion posicion valor unMicroprocesador = unMicroprocesador {datos = (primeraParteDeLosDatos (posicion) (datos unMicroprocesador)) ++ (segundaParteDeLosDatos posicion valor (datos unMicroprocesador))}

divide :: NuevoMicroprocesador
divide unMicroprocesador
   | acumuladorB unMicroprocesador == 0 = agregarErrorBy0 unMicroprocesador
   | otherwise = (incrementarPc.dividirAcumuladores) unMicroprocesador



--Otras funciones


incrementarPc :: NuevoMicroprocesador
incrementarPc unMicroprocesador = unMicroprocesador {pc = pc unMicroprocesador + 1} 

sumarValores:: Int->NuevoMicroprocesador 
sumarValores  valor = swap.(lodv valor)

dividirAcumuladores :: NuevoMicroprocesador
dividirAcumuladores unMicroprocesador =  unMicroprocesador{acumuladorA = div (acumuladorA unMicroprocesador) (acumuladorB unMicroprocesador), acumuladorB =0 }

antesDeDividir ::Int->Int->NuevoMicroprocesador
antesDeDividir numerador denominador = (lod 1).swap.(lod 2).(str 2 denominador).(str 1 numerador) 


agregarErrorBy0 :: NuevoMicroprocesador
agregarErrorBy0 unMicroprocesador = unMicroprocesador {ultimoError = "ERROR DIVISION BY ZERO" }

resultadoDeDividirPor0 ::Int->Int->NuevoMicroprocesador
resultadoDeDividirPor0 numerador denominador = incrementarPc.agregarErrorBy0.(antesDeDividir numerador denominador)

memoriaVacia :: NuevoMicroprocesador
memoriaVacia unMicroprocesador = unMicroprocesador{datos = replicate 0 1024}

primeraParteDeLosDatos :: Int -> [Int] -> [Int]
primeraParteDeLosDatos posicion lista = take (posicion - 1) (lista)

segundaParteDeLosDatos :: Int -> Int -> [Int] -> [Int]
segundaParteDeLosDatos posicion valor lista = valor:(drop (posicion) (lista))

-- Programas      
                                                                
avanzarTresPosicionesPc :: NuevoMicroprocesador
avanzarTresPosicionesPc = nop.nop.nop                                                 

programaParaSumar :: Int -> Int-> NuevoMicroprocesador
programaParaSumar  valor1 valor2 = add.(lodv valor1).(sumarValores valor2) 

programaParaDividir ::Int->Int->NuevoMicroprocesador
programaParaDividir numerador denominador = divide.(antesDeDividir numerador denominador)
