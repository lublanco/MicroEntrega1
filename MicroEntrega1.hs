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
intercambiarValores unMicroprocesador = unMicroprocesador {acumuladorA = acumuladorB unMicroprocesador, acumuladorB = acumuladorA unMicroprocesador}                                                 

lodv :: Int-> NuevoMicroprocesador
lodv valor = incrementarPc.(cargarUnValorEnElAcumuladorA valor)

cargarUnValorEnElAcumuladorA :: Int->NuevoMicroprocesador 
cargarUnValorEnElAcumuladorA  valor unMicroprocesador = unMicroprocesador{acumuladorA = valor}

lod :: Int->NuevoMicroprocesador
lod numero = incrementarPc.(cargarElContenidoDeDatosEnAcumuladorA numero)

cargarElContenidoDeDatosEnAcumuladorA :: Int -> NuevoMicroprocesador 
cargarElContenidoDeDatosEnAcumuladorA numero unMicroprocesador = unMicroprocesador {acumuladorA = head (recortarPosicionesMenosUno numero  unMicroprocesador)}

recortarPosicionesMenosUno :: Int -> Microprocesador -> [Int]
recortarPosicionesMenosUno numero  unMicroprocesador = drop (numero - 1) (datos unMicroprocesador)

str :: Int->Int->NuevoMicroprocesador
str posicion valor = incrementarPc.(guardaElValorEnLaPosicion posicion valor)

guardaElValorEnLaPosicion :: Int->Int->NuevoMicroprocesador
guardaElValorEnLaPosicion posicion valor unMicroprocesador = unMicroprocesador {datos =  (take (posicion - 1) (datos unMicroprocesador)) ++ ([valor]) ++ (drop (posicion) (datos unMicroprocesador))}


divide :: NuevoMicroprocesador
divide = incrementarPc.dividirAcumuladores



--Otras funciones


incrementarPc :: NuevoMicroprocesador
incrementarPc unMicroprocesador = unMicroprocesador {pc = pc unMicroprocesador + 1} 

sumarValores:: Int->NuevoMicroprocesador 
sumarValores  valor = swap.(lodv valor)

dividirAcumuladores :: NuevoMicroprocesador
dividirAcumuladores unMicroprocesador =  unMicroprocesador{acumuladorA = div (acumuladorA unMicroprocesador) (acumuladorB unMicroprocesador), acumuladorB =0 }

transformarMicroprocesador ::Int->Int->NuevoMicroprocesador
transformarMicroprocesador numerador denominador = (lod 1).swap.(lod 2).(str 2 denominador).(str 1 numerador) 

agregarErrorBy0 :: NuevoMicroprocesador
agregarErrorBy0 unMicroprocesador = unMicroprocesador {ultimoError = "ERROR DIVISION BY ZERO" }

resultadoDeDividirPor0 ::Int->Int->NuevoMicroprocesador
resultadoDeDividirPor0 numerador denominador = incrementarPc.agregarErrorBy0.(transformarMicroprocesador numerador denominador)


-- Programas      
                                                                
avanzarTresPosicionesPc :: NuevoMicroprocesador
avanzarTresPosicionesPc = nop.nop.nop                                                 

programaParaSumar :: Int -> Int-> NuevoMicroprocesador
programaParaSumar  valor1 valor2 = add.(lodv valor1).(sumarValores valor2) 

programaParaDividir ::Int->Int->NuevoMicroprocesador
programaParaDividir numerador denominador 
   | denominador == 0 = resultadoDeDividirPor0 numerador denominador
   | otherwise = divide.(transformarMicroprocesador numerador denominador)





-----------------------------------------------------------------------------------------------------------
{-Accessors 

programCounter (Microprocesador _ _ pc _ _) = pc                                                                     
  
acumuladorA (Microprocesador acumuladorA _ _ _ _) = acumuladorA      
  
acumuladorB (Microprocesador _ acumuladorB _ _ _) = acumuladorB      
  
memoria (Microprocesador _ _ _ posicion _) = posicion    
  
mensajeError(Microprocesador _ _ _ _ errores) = errores  

-}

