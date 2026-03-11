module Practica02 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"


p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

type Estado = [String]

--Funcion auxiliar
conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia (x:xs) = [(x:ys) | ys <- conjPotencia xs] ++ conjPotencia xs

--ELimina variables repetidas
eliminarDuplicados:: Eq a => [a]->[a]
eliminarDuplicados [] = []
eliminarDuplicados (x:xs) = x : eliminarDuplicados [y | y <-xs, y/=x]   --Toma el primer elemento 'x'
                                                                        --Cada elemento 'xs' lo llama 'y', solo los deja pasar si son diferentes de 'x'
                                                                        --Llama recusivamente a la funcion eliminarDuplicados sobre 'y'

--Busca si una variable esta en la lista
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece variable_Buscar (x:xs) = if variable_Buscar == x             --Compara la variable que busco con la del primer elemento de la lista
                                    then True                          --Encontro la variable que busco
                                    else pertenece variable_Buscar xs  --Si no la encuentra, volvemos a llamar al metodo sobre el resto de la lista 

--EJERCICIOS

--Ejercicio 1
variables :: Prop -> [String]
variables prop = eliminarDuplicados (varAuxiliar prop)              --Llama la funcion auxiliar para conseguir la lista de las variables, despues elimina los duplicados
    where
    varAuxiliar (Cons _) = []
    varAuxiliar (Var a) = [a]                                       --Extrae la variable y la guarda en una lista
    varAuxiliar (Not a) = varAuxiliar a                             --En los demas, tambien extrae las variables de las formulas y las concatena en una lista
    varAuxiliar (Or a b) = varAuxiliar a ++ varAuxiliar b
    varAuxiliar (And a b) = varAuxiliar a ++ varAuxiliar b
    varAuxiliar (Impl a b) = varAuxiliar a ++ varAuxiliar b 
    varAuxiliar (Syss a b) = varAuxiliar a ++ varAuxiliar b 


--Ejercicio 2
interpretacion :: Prop -> Estado -> Bool
interpretacion (Cons b) _ = b                                                  --Las contantes siempre tien su mismo valor, no importa el estado que se pase
interpretacion (Var a) i = pertenece a i                                       --Si es una variable, llama a la funcion 'pertenece' para revisar si la variable esta enla lista del estado 'i', es verdad si pertenece a 'i' 
interpretacion (Not a) i = not(interpretacion a i)                             --Llama la funcion interpretacion para la variable 'a' y despues la niega
interpretacion (Or a b) i = interpretacion a i || interpretacion b i           --Llama la funcion interpretacion para 'a' y 'b' y despues las las une con ||, es verdad si al menos una de las dos es verdad
interpretacion (And a b) i = interpretacion a i && interpretacion b i          --Llama la funcion interpretacion para 'a' y 'b' y despues las las une con &&, es verdad si las dos son verdad
interpretacion (Impl a b) i = not(interpretacion a i) || interpretacion b i    --Llama la funcion interpretacion para 'a' y 'b' y despues niega 'a' y las las une con ||, es verdad si al menos una de las dos es verdad
interpretacion (Syss a b) i = interpretacion a i == interpretacion b i         --Llama la funcion interpretacion para 'a' y 'b' y despues compara si sus interpretaciones son iguales, es verdad si son iguales

--Ejercicio 3
estadosPosibles :: Prop -> [Estado]
estadosPosibles prop = conjPotencia (variables prop) --Primero extrae todas las variables de la formula con variables. Luego le saca el conjPotencia. Esto da todas las combinaciones posibles de Verdadero y Falso.

--Ejercicio 4
modelos :: Prop -> [Estado]
modelos prop = [i | i <- estadosPosibles prop, interpretacion prop i ] --Da cada estado 'i' extraido de estadosPosibles, tal que al evalucar la formula en interpretacion 'i' el resultado sea Verdadero

--Ejercicio 5
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes f1 f2 = verifica estados
    where 
    varUnidas = eliminarDuplicados (variables f1 ++ variables f2)      --JUnta todas las variables de las dos formulas  y elimina las que esten duplicadas
    estados = conjPotencia varUnidas                                   --Saca todos los posibles estados de las variables unidas
                                                                       --La funcion verfica, recorre todos los estados
    verifica [] = True                                                 --Termina de revisar todo, devuleve True
    verifica (i:is) = if interpretacion f1 i == interpretacion f2 i    --'verifica' resive la lista de los estados, a cada formula saca la interpretacion en el primer estado, compara si son iguales
    then verifica is                                                   --Si son iguales, continua comparando con los demas estados
    else False                                                         --Si no son iguales se corta y no son equivalentes

--Ejercicio 6 
tautologia :: Prop -> Bool
tautologia prop = modelos prop == estadosPosibles prop      --Es verdadera siempre. Asi que la lista de casos donde es verdadera (modelos) es exactamente igual a la lista de todos los casos posibles.

--Ejercicio 7
contradiccion :: Prop -> Bool
contradiccion prop = modelos prop == []                     --Nunca es verdadera. Su lista de modelos esta vacia []

--Ejercicio 8
consecuenciaLogica :: [Prop] -> Prop -> Bool
consecuenciaLogica premisas conclusion = verifica estado
    
    where

    varLista [] = []
    varLista (x:xs) = variables x ++ varLista xs                                        --Extra las variables del primer elemento de la lista 'x', despues vuelve a llamar a la funcion sobre los demas elementos de 'xs' y los concatena

    todasVariables = eliminarDuplicados (varLista premisas ++ variables conclusion)     --Creamos una gran lista con las variables de las premisas y la conclusion, y eliminamos las variables duplicadas
    
    estado = conjPotencia todasVariables                                                --Sacamos la lista de todos los estados posibles de la lista de todas las variables
 
    esModeloLista _ [] = True                                                           
    esModeloLista i (x:xs) = interpretacion x i && esModeloLista i xs                   --Recibe un estado 'i' y la lista de las premisas, saca la interpretacion de la primera formula de las premisas, despues se vuelve a llamar para hacer lo mismo con el resto de elementos, tienen que ser verdad todas para que sea verdad

    verifica [] = True
    verifica (i:is) = if esModeloLista i premisas                                       --Los usamos para revisar que todas las premisas sean verdad en el primer estado
                    then if  interpretacion conclusion i                                --Si las premisas son verdad en ese estado, checa que la conclusion sea verdad en el mismo estado
                        then verifica is                                                   --Si la conclusion es verdad en ese estado, paso a los demas estados resursivamente
                        else False                                                         --Si la conclusion no es verdad en ese estado, devuelve falso
                    else verifica is                                                    --Si las premisas son falsas en el primer estado, no importa la conclusion, asi que continua en los demas estados


