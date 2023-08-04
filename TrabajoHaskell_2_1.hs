import System.Random

data Particula = P [Float] [Float] [Float] Float --En orden de aparicion: Posicion actual - Velocidad actual - Mejor posicion propia - Valor de la mejor posicion propia
   deriving (Read, Show)
data Enjambre = E [Particula] Particula --El conjunto de particulas y la particula mejor global
   deriving (Read, Show)
   
--------------FUNCIONES PREDEFINIDAS PARA EL MAIN-------------

g::[Float]->Float
g (x:y:[]) = -2**(-((x-6)*(x-6))-(y*y))

f::[Float]->Float
f (x:y:[]) = sin x + sin y

h::[Float]->Float
h (x:y:z:[]) = x*x+y*y+z*z

l::[Float]->Float
l (x:y:[]) = (x*x+y*y)/(x*x+1)


--------------NUMEROS ALEATORIOS-------------------

randoms'::(RandomGen g, Random a) => g -> [a]
randoms' g = x:randoms' g'
   where (x, g') = random g
   
{-Necesitamos saber cuantos numeros aleatorios vamos a necesitar en cada caso
dim = length rectangulo
Para inicializar: k*dim (para las posiciones) k*dim (para las velocidades)
Para cada iteracion: 2 (para los dos numeros aleatorios entre 0 y 1) por cada particula
Total: 2*k*dim + 2*k*n
-}


-----------------------------------------------------------------------
-----------------OPTIMIZACION PSO--------------------------------------
-----------------------------------------------------------------------


--Funcion que obtiene todos los datos del proceso en un String del proceso de PSO, dependiendo de la opcion, un formato u otro
optimizarPSO_datos::([Float]->Float)->[(Float, Float)]->Float->Float->Float->Int->Int->Int->Int->String --Toma como entrada la funcion a optimizar, rectangulo dominio, parametro de peso inercial w, peso de la direccion hacia la mejor propia c1, peso hacia la mejor global c2, numero de iteraciones, numero de particulas, entero semilla para la aleatoriedad, entero opcion para el formato de guardado en fichero
optimizarPSO_datos f rectangulo w c1 c2 n k semilla opcion = optimizar_auxPSO_datos f w c1 c2 n k dr opcion "" (inicializa_enjambre f rectangulo k iz)
   where nums_al = take (2*k*dim + 2*k*n) (randoms'(mkStdGen semilla) :: [Int])
         dim = length rectangulo
         (iz, dr) = splitAt (2*k*dim) nums_al

optimizar_auxPSO_datos::([Float]->Float)->Float->Float->Float->Int->Int->[Int]->Int->String->Enjambre->String
optimizar_auxPSO_datos f w c1 c2 n k semillas opcion datos (E enj p)
   |n==0 = (show p)++datos++"\n"++(show' opcion (E enj p)) --Cambia el show para elegir formato
   |otherwise = optimizar_auxPSO_datos f w c1 c2 (n-1) k dr opcion (datos++"\n"++(show' opcion (E enj p))) enj_nuevo --Cambia el show para elegir el formato
   where (iz, dr) = splitAt (2*k) semillas
         enj_nuevo = actualiza_enjPSO f w c1 c2 iz (E enj p)

show'::Int->Enjambre->String --Por si queremos tener solo las posiciones de las particulas en el archivo de texto para tratarlas en otro programa
show' opcion (E enj p)
   |opcion==1 = dame_pos_enj enj
   |otherwise = show (E enj p)

---Te devuelve un string con las posiciones del enjambre
dame_pos_enj::[Particula]->String
dame_pos_enj [] = ""
dame_pos_enj ((P pos _ _ _):xs) = (elems_lista pos)++(dame_pos_enj xs)

elems_lista::[Float]->String
elems_lista [] = ""
elems_lista (x:xs) = (show x)++" "++(elems_lista xs)

--Funcion que obtiene todos los datos del proceso en un String del proceso de PSO2, dependiendo de la opcion, un formato u otro
optimizarPSO_datos2::([Float]->Float)->[(Float, Float)]->Float->Float->Float->Int->Int->Int->Int->String --Toma como entrada la funcion a optimizar, rectangulo dominio, parametro de peso inercial w, peso de la direccion hacia la mejor propia c1, peso hacia la mejor global c2, numero de iteraciones, numero de particulas, entero semilla para la aleatoriedad, entero opcion para el formato de guardado en fichero
optimizarPSO_datos2 f rectangulo w c1 c2 n k semilla opcion = optimizar_auxPSO_datos2 f rectangulo w c1 c2 n k dr opcion "" (inicializa_enjambre f rectangulo k iz)
   where nums_al = take (2*k*dim + 2*k*n) (randoms'(mkStdGen semilla) :: [Int])
         dim = length rectangulo
         (iz, dr) = splitAt (2*k*dim) nums_al

optimizar_auxPSO_datos2::([Float]->Float)->[(Float, Float)]->Float->Float->Float->Int->Int->[Int]->Int->String->Enjambre->String
optimizar_auxPSO_datos2 f rectangulo w c1 c2 n k semillas opcion datos (E enj p)
   |n==0 = (show p)++datos++"\n"++(show' opcion (E enj p)) --Cambia el show para elegir formato
   |otherwise = optimizar_auxPSO_datos2 f rectangulo w c1 c2 (n-1) k dr opcion (datos++"\n"++(show' opcion (E enj p))) enj_nuevo --Cambia el show para elegir el formato
   where (iz, dr) = splitAt (2*k) semillas
         enj_nuevo = actualiza_enjPSO2 f rectangulo w c1 c2 iz (E enj p)

--Funcion que te da la particula que alcanzo el minimo sin limite del rectangulo
optimizarPSO::([Float]->Float)->[(Float, Float)]->Float->Float->Float->Int->Int->Int->Particula --Toma como entrada la funcion a optimizar, rectangulo dominio, parametro de peso inercial w, peso de la direccion hacia la mejor propia c1, peso hacia la mejor global c2, numero de iteraciones, numero de particulas, entero semilla para la aleatoriedad y devuelve la particula con mejor valor
optimizarPSO f rectangulo w c1 c2 n k semilla = optimizar_auxPSO f w c1 c2 n k dr (inicializa_enjambre f rectangulo k iz)
   where nums_al = take (2*k*dim + 2*k*n) (randoms'(mkStdGen semilla) :: [Int])
         dim = length rectangulo
         (iz, dr) = splitAt (2*k*dim) nums_al --Divido la lista de semillas en dos, para tener para la inicializacion y para las iteraciones

------------INICIALIZACION------------

---Crea un enjambre aleatorio repartido por el rectangulo, usando particulas_iniciales como funcion auxiliar
inicializa_enjambre::([Float]->Float)->[(Float, Float)]->Int->[Int]->Enjambre --Toma como entrada: funcion, rectangulo, num part, lista semillas Devuelve: Enjambre
inicializa_enjambre f rectangulo k semillas = (E particulas (mejor_global particulas))
   where particulas = particulas_iniciales f rectangulo k semillas

---Dado un enjambre, devuelve a particula con valor minimo
mejor_global::[Particula]->Particula
mejor_global (x:[]) = x
mejor_global ((P pos vel mejpos mejval):xs)
   |mejval <= mejval' = (P pos vel mejpos mejval) --Minimizando
   |otherwise = (P pos' vel' mejpos' mejval')
   where (P pos' vel' mejpos' mejval') = mejor_global xs


particulas_iniciales::([Float]->Float)->[(Float, Float)]->Int->[Int]->[Particula] ----Toma como entrada: funcion, rectangulo, num part, lista semillas Devuelve: lista de particula
particulas_iniciales f rectangulo k semillas
   |k==0 = []
   |otherwise = nueva_p:(particulas_iniciales f rectangulo (k-1) dr) --Las semillas a usar en la siguiente iteracion seran semillas menos las 2*dim primeras que se han usado para hallar la pos y vel de una particula nueva
   where dim = length rectangulo
         (iz, dr) = splitAt (2*dim) semillas
         (semillas_pos, semillas_vel) = splitAt dim iz 
         nueva_p = (P nueva_pos (genera_vel dim semillas_vel) nueva_pos (f nueva_pos))
         nueva_pos = genera_pos rectangulo semillas_pos

genera_pos::[(Float, Float)]->[Int]->[Float] --ENTRADA: rectangulo, lista de semillas SALIDA: posicion aleatoria en el rectangulo 
genera_pos rectangulo semillas_pos = map fst (zipWith randomR rectangulo (map mkStdGen semillas_pos)) 

genera_vel::Int->[Int]->[Float] --ENTRADA: dimension, lista de semillas, SALIDA: velocidad modulo 1 aleatoria
genera_vel dim semillas_vel = normalizaV (genera_pos (take dim (repeat (-1,1))) semillas_vel)

---Normaliza un vector---
normalizaV::[Float]->[Float]
normalizaV xs = map (/norm) xs
   where norm = sqrt (sum (map (^2) xs))

-------------ITERACION------------------

optimizar_auxPSO::([Float]->Float)->Float->Float->Float->Int->Int->[Int]->Enjambre->Particula
optimizar_auxPSO f w c1 c2 n k semillas (E enj p)
   |n==0 = p
   |otherwise = optimizar_auxPSO f w c1 c2 (n-1) k dr (actualiza_enjPSO f w c1 c2 iz (E enj p))
   where (iz, dr) = splitAt (2*k) semillas

---Actualiza el enjambre respecto a la iteracion anterior ----   
actualiza_enjPSO::([Float]->Float)->Float->Float->Float->[Int]->Enjambre->Enjambre
actualiza_enjPSO f w c1 c2 semillas (E enj p) = (E new_enj new_p) --enj es la lista de particulas
   where new_enj = zipWith (refresca_par f w c1 c2 p) enj pares_semillas --refresca_enjPSO f w c1 c2 semillas enj
         pares_semillas = empareja semillas
         new_p = mejor_global new_enj

empareja::[Int]->[(Int, Int)]
empareja [] = []
empareja (x:y:xs) = (x,y):(empareja xs)

refresca_par::([Float]->Float)->Float->Float->Float->Particula->Particula->(Int, Int)->Particula
refresca_par f w c1 c2 (P _ _ mej_pos_glob _) (P pos vel mej_pos mej) (sem1, sem2) = (P new_pos new_vel new_pos_mej new_mej)
   where r1 = fst (randomR (0, 1) (mkStdGen sem1))
         r2 = fst (randomR (0, 1) (mkStdGen sem2))
         new_vel = sumaV (sumaV (map (*w) vel) (map (*(c1*r1)) (restaV mej_pos pos))) (map (*(c2*r2)) (restaV mej_pos_glob pos))
         new_pos = sumaV new_vel pos
         (new_pos_mej, new_mej) = minimoPosVal (mej_pos, mej) (new_pos, (f new_pos))

minimoPosVal::(a, Float)->(a, Float)->(a,Float)
minimoPosVal (p, x) (q, y)
   |x<=y = (p, x)
   |otherwise= (q,y)

sumaV::[Float] -> [Float] -> [Float]
sumaV xs ys = zipWith (+) xs ys

restaV::[Float] -> [Float] -> [Float]
restaV xs ys = zipWith (-) xs ys

--Funcion que te da la particula que alcanzo el minimo con limite del rectangulo
optimizarPSO2::([Float]->Float)->[(Float, Float)]->Float->Float->Float->Int->Int->Int->Particula --Toma como entrada la funcion a optimizar, rectangulo dominio, parametro de peso inercial w, peso de la direccion hacia la mejor propia c1, peso hacia la mejor global c2, numero de iteraciones, numero de particulas, entero semilla para la aleatoriedad y devuelve la particula con mejor valor
optimizarPSO2 f rectangulo w c1 c2 n k semilla = optimizar_auxPSO2 f rectangulo w c1 c2 n k dr (inicializa_enjambre f rectangulo k iz)
   where nums_al = take (2*k*dim + 2*k*n) (randoms'(mkStdGen semilla) :: [Int])
         dim = length rectangulo
         (iz, dr) = splitAt (2*k*dim) nums_al --Divido la lista de semillas en dos, para tener para la inicializacion y para las iteraciones


optimizar_auxPSO2::([Float]->Float)->[(Float, Float)]->Float->Float->Float->Int->Int->[Int]->Enjambre->Particula
optimizar_auxPSO2 f rectangulo w c1 c2 n k semillas (E enj p)
   |n==0 = p
   |otherwise = optimizar_auxPSO2 f rectangulo w c1 c2 (n-1) k dr (actualiza_enjPSO2 f rectangulo w c1 c2 iz (E enj p))
   where (iz, dr) = splitAt (2*k) semillas
   
actualiza_enjPSO2::([Float]->Float)->[(Float, Float)]->Float->Float->Float->[Int]->Enjambre->Enjambre
actualiza_enjPSO2 f rectangulo w c1 c2 semillas (E enj p) = (E new_enj new_p) --enj es la lista de particulas
   where new_enj = zipWith (refresca_par2 f rectangulo w c1 c2 p) enj pares_semillas--refresca_enjPSO f w c1 c2 semillas enj
         pares_semillas = empareja semillas
         new_p = mejor_global new_enj

refresca_par2::([Float]->Float)->[(Float, Float)]->Float->Float->Float->Particula->Particula->(Int, Int)->Particula
refresca_par2 f rectangulo w c1 c2 (P _ _ mej_pos_glob _) (P pos vel mej_pos mej) (sem1, sem2) = (P new_pos2 new_vel new_pos_mej new_mej)
   where r1 = fst (randomR (0, 1) (mkStdGen sem1))
         r2 = fst (randomR (0, 1) (mkStdGen sem2))
         new_vel = sumaV (sumaV (map (*w) vel) (map (*(c1*r1)) (restaV mej_pos pos))) (map (*(c2*r2)) (restaV mej_pos_glob pos))
         new_pos2 = zipWith ajuste (sumaV new_vel pos) rectangulo
         (new_pos_mej, new_mej) = minimoPosVal (mej_pos, mej) (new_pos2, (f new_pos2))

---Para que no salgan del rectangulo---
ajuste::Float->(Float,Float)->Float
ajuste a (b,c)
   |a<b = b
   |a>c = c
   |otherwise = a
  

-----------------------------------------------------------------------
-----------------OPTIMIZACION ACO VIAJANTE-----------------------------
-----------------------------------------------------------------------

data Hormiga = H [Int] Float --La lista de enteros nos dice los nodos por donde va la hormiga y el float el valor de coste
   deriving (Show, Read)
data Colonia = C [Hormiga] Hormiga --La hormiga se refiere a la que mejor camino ha obtenido entre todas klas demas de la colonia
   deriving (Show, Read)
   
--Funcion que guarda en un string la matriz donde cada fila estan las secuencias de viaje de cada hormiga (1,3,2 --no se pone el inicio al final--)
optimizarACO_V_str::[[Float]]->Float->Float->Float->Float->Float->Int->Int->Int->Int->String
optimizarACO_V_str costes p alfa beta q benef k n s modo = optimizarACO_V_aux_str costes p alfa beta q benef k n ferom atract semillas modo ""
   where semillas = take (n*k*(num_nod-1)) (randoms'(mkStdGen s) :: [Int]) --Las veces que vamos a llamar a random es n*k*(n-1)
         ferom = matriz_de_elem num_nod 1.0
         atract = anula_col 1 (zipWith (fila_atract_inicial 1) costes [1..num_nod])
         num_nod = length costes

optimizarACO_V_aux_str::[[Float]]->Float->Float->Float->Float->Float->Int->Int->[[Float]]->[[Float]]->[Int]->Int->String->String
optimizarACO_V_aux_str costes p alfa beta q benef k n ferom atract semillas modo result
   |n==1 = show mej_hormiga++result++"\n"++colonia_str
   |otherwise = optimizarACO_V_aux_str costes p alfa beta q benef k (n-1) new_ferom atract sem2 modo (result++"\n"++colonia_str)
   where (sem1, sem2) = splitAt (k*(num_nod-1)) semillas
         num_nod = length ferom
         (C hormigas mej_hormiga) = dame_colonia costes alfa beta k ferom atract sem1
         new_ferom = actualiza_ferom ferom hormigas p q benef
         colonia_str = procesa_col (C hormigas mej_hormiga) modo
         
procesa_col::Colonia->Int->String
procesa_col (C hormigas mej_hormiga) modo
   |modo==1 = show (C hormigas mej_hormiga)
   |otherwise = concat $ map procesa_hor hormigas

procesa_hor::Hormiga->String
procesa_hor (H xs _) = procesa_ints xs

procesa_ints::[Int]->String
procesa_ints (x:xs) = (show x) ++ (' ':procesa_ints xs)
procesa_ints [] = ""
 

--Funcion que optimiza problema del viajante saliendo del nodo 1
optimizarACO_V::[[Float]]->Float->Float->Float->Float->Float->Int->Int->Int->Colonia --ENTRADA: lista de los costes, coeficiente evaporacion (0<p<1), alfa >= 0, beta >=1, q >0, numero de hormigas, numero de iteraciones, semilla SALIDA: Hormiga
optimizarACO_V costes p alfa beta q benef k n s = optimizarACO_V_aux costes p alfa beta q benef k n ferom atract semillas
   where semillas = take (n*k*(num_nod-1)) (randoms'(mkStdGen s) :: [Int]) --Las veces que vamos a llamar a random es n*k*(n-1)
         ferom = matriz_de_elem num_nod 1.0
         atract = anula_col 1 (zipWith (fila_atract_inicial 1) costes [1..num_nod])
         num_nod = length costes
         
---INICIAR Y ACTUALIZAR ATRACT---

anula_col::Int->[[Float]]->[[Float]] --Contando desde 1
anula_col n xss = map (pon_cero n) xss

pon_cero::Int->[Float]->[Float]
pon_cero n xs = (take (n-1) iz)++(0:dr)
   where (iz, dr) = splitAt n xs

fila_atract_inicial::Int->[Float]->Int->[Float]
fila_atract_inicial pos xs num_fila
   |pos==num_fila = 0:fila_atract_inicial (pos+1) xs num_fila
   |pos>length xs = []
   |otherwise = (1/(xs!!(pos-1))):fila_atract_inicial (pos+1) xs num_fila
------------------------------------------------

optimizarACO_V_aux::[[Float]]->Float->Float->Float->Float->Float->Int->Int->[[Float]]->[[Float]]->[Int]->Colonia
optimizarACO_V_aux costes p alfa beta q benef k n ferom atract semillas
   |n==1 = (C hormigas mej_hormiga)
   |otherwise = optimizarACO_V_aux costes p alfa beta q benef k (n-1) new_ferom atract sem2
   where (sem1, sem2) = splitAt (k*(num_nod-1)) semillas
         num_nod = length ferom
         (C hormigas mej_hormiga) = dame_colonia costes alfa beta k ferom atract sem1
         new_ferom = actualiza_ferom ferom hormigas p q benef
         
------------ACTUALIZAR FEROM-------------------

actualiza_ferom::[[Float]]->[Hormiga]->Float->Float->Float->[[Float]]
actualiza_ferom ferom hs p q benef = foldr (sumaM) (matriz_de_elem num_nod 0.0) ((map (map (*(1-p))) ferom):feroms_por_hormiga) --Con el map map hacemos el paso de evaporacion, luego feroms_por_hormiga es la matriz a sumar por hormiga debido a las feromonas que aporta
   where num_nod = length ferom
         feroms_por_hormiga = map (dame_matriz_ferom q benef) hs

dame_matriz_ferom::Float->Float->Hormiga->[[Float]] --Recuerda, ns es el trayecto desde 1 hasta la ultima ciudad (sin volver a 1)
dame_matriz_ferom q benef (H ns c) = foldr (cambia_elem (benef*q/c)) (matriz_de_elem num_nod 0.0) (zip ns ((tail ns)++[1])) --c lleva el coste de retorno ya a la pos 1
   where num_nod = length ns

cambia_elem::Float->(Int,Int)->[[Float]]->[[Float]]
cambia_elem f (i,j) xss = (take (i-1) xss)++[(take (j-1) filai)++[f]++(drop j filai)]++(drop i xss)
   where filai = xss!!(i-1)

sumaM::[[Float]] -> [[Float]] -> [[Float]]
sumaM xs ys = zipWith (sumaV) xs ys

--Te crea una matriz de dim n con el elemento x en todas las posiciones
matriz_de_elem::Int->Float->[[Float]] 
matriz_de_elem n x = take n (repeat (take n (repeat x)))

----------------------------------------------
     
dame_colonia::[[Float]]->Float->Float->Int->[[Float]]->[[Float]]->[Int]->Colonia
dame_colonia costes alfa beta k ferom atract sems = C hormigas mej_hormiga
   where hormigas = dame_colonia_aux costes alfa beta k ferom atract sems
         mej_hormiga = best_hormiga hormigas

dame_colonia_aux::[[Float]]->Float->Float->Int->[[Float]]->[[Float]]->[Int]->[Hormiga]
dame_colonia_aux _ _ _ 0 _ _ _ = []
dame_colonia_aux costes alfa beta k ferom atract sems = (dame_hormiga costes alfa beta ferom atract iz [1]):(dame_colonia_aux costes alfa beta (k-1) ferom atract dr)
   where (iz,dr) = splitAt ((length ferom)-1) sems

best_hormiga:: [Hormiga]->Hormiga
best_hormiga ((H ns1 c1):[]) = (H ns1 c1)
best_hormiga ((H ns1 c1):hs)
   |c1<c2 = (H ns1 c1)
   |otherwise = (H ns2 c2)
   where (H ns2 c2) = best_hormiga hs


dame_hormiga::[[Float]]->Float->Float->[[Float]]->[[Float]]->[Int]->[Int]->Hormiga --HAY QUE INICIALIZAR acum CUANDO LLAMEMOS A dame_hormiga CON [1]
dame_hormiga costes alfa beta ferom atract sems acum
   |long_acum == (length ferom) = (H acum (coste_camino acum costes))
   |otherwise = dame_hormiga costes alfa beta ferom new_atract (tail sems) (acum++[sig_paso]) 
   where long_acum = length acum
         sig_paso = dame_paso alfa beta ferom atract (head sems) (acum !! (long_acum-1))
         new_atract = anula_col sig_paso atract

matriz_elem::Int->Int->[[Float]]->Float
matriz_elem i j xss = (xss!!(i-1))!!(j-1)

coste_camino::[Int]->[[Float]]->Float --Tiene en cuenta el coste de retorno a la ciudad 1
coste_camino (x:y:ys) xs = (matriz_elem x y xs)+(coste_camino (y:ys) xs)
coste_camino (x:[]) xs = matriz_elem x 1 xs


dame_paso::Float->Float->[[Float]]->[[Float]]->Int->Int->Int
dame_paso alfa beta ferom atract sem paso_anterior = dame_paso_aux ((0.0, -1):(scanl1 (sum_float_pos) (filter ((/=0).fst) (zip probs [1..])))) (fst (randomR (0, 1) (mkStdGen sem)))
   where probs = map (/sumatorio_prob) fila_probs
         sumatorio_prob = sum fila_probs
         fila_probs = zipWith (prod22 alfa beta) (ferom !! (paso_anterior - 1)) (atract !! (paso_anterior-1))
         
prod22:: Float->Float->Float->Float->Float
prod22 alfa beta x y
   |(x==0)||(y==0) = 0
   |otherwise = (x**alfa) * (y**beta)

sum_float_pos::(Float, Int)->(Float, Int)->(Float, Int)
sum_float_pos (x1, n1) (x2, n2) = (x1+x2, n2)

dame_paso_aux::[(Float, Int)]->Float->Int --Como nunca va a llegar a la lista vacia no ponemos caso base. Toma como entrada el zip de las probs acumuladas con las posiciones asociadas
dame_paso_aux ((x1, n1):(x2, n2):xs) num_al
   |(x1<=num_al)&&(num_al<=x2) = n2
   |otherwise = dame_paso_aux ((x2,n2):xs) num_al
   
----METODO ACO GENERAL, para cualquier grafo----  

optimizarACO_V2::[[Float]]->Float->Float->Float->Float->Float->Int->Int->Int->Colonia --ENTRADA: lista de los costes, coeficiente evaporacion (0<p<1), alfa >= 0, beta >=1, q >0, benef>=1, numero de hormigas, numero de iteraciones, semilla SALIDA: Hormiga
optimizarACO_V2 costes p alfa beta q benef k n s = optimizarACO_V_aux2 costes p alfa beta q benef k n ferom atract semillas
   where semillas = take (n*k*(num_nod-1)) (randoms'(mkStdGen s) :: [Int]) --Las veces que vamos a llamar a random es n*k*(n-1)
         ferom = matriz_de_elem num_nod 1.0 
         atract = anula_col 1 (map (map (1/)) costes)  ---BIEN---
         num_nod = length costes

optimizarACO_V_aux2::[[Float]]->Float->Float->Float->Float->Float->Int->Int->[[Float]]->[[Float]]->[Int]->Colonia
optimizarACO_V_aux2 costes p alfa beta q benef k n ferom atract semillas
   |n==1 = (C hormigas mej_hormiga)
   |otherwise = optimizarACO_V_aux2 costes p alfa beta q benef k (n-1) new_ferom atract sem2
   where (sem1, sem2) = splitAt (k*(num_nod-1)) semillas
         num_nod = length ferom
         (C hormigas mej_hormiga) = dame_colonia2 costes alfa beta k ferom atract sem1
         new_ferom = actualiza_ferom2 ferom hormigas p q benef
         
actualiza_ferom2::[[Float]]->[Hormiga]->Float->Float->Float->[[Float]]
actualiza_ferom2 ferom hs p q benef = foldr (sumaM) (matriz_de_elem num_nod 0.0) ((map (map (*(1-p))) ferom):feroms_por_hormiga) --Con el map map hacemos el paso de evaporacion, luego feroms_por_hormiga es la matriz a sumar por hormiga debido a las feromonas que aporta
   where num_nod = length ferom
         feroms_por_hormiga = map (dame_matriz_ferom2 ferom q benef mej_c) hs
         (H _ mej_c) = best_hormiga hs

dame_matriz_ferom2::[[Float]]->Float->Float->Float->Hormiga->[[Float]] --Recuerda, ns es el trayecto desde 1 hasta la ultima ciudad (sin volver a 1)
dame_matriz_ferom2 ferom q benef mej_c (H ns c)
    | c==(1/0) = (matriz_de_elem num_nod 0.0)
    | mej_c == c = foldr (cambia_elem (benef*q/c)) (matriz_de_elem num_nod 0.0) (zip ns (tail ns)) 
    | otherwise = foldr (cambia_elem (q/c)) (matriz_de_elem num_nod 0.0) (zip ns (tail ns)) 
   where num_nod = length ferom

dame_colonia2::[[Float]]->Float->Float->Int->[[Float]]->[[Float]]->[Int]->Colonia
dame_colonia2 costes alfa beta k ferom atract sems = C hormigas mej_hormiga
   where hormigas = dame_colonia_aux2 costes alfa beta k ferom atract sems
         mej_hormiga = best_hormiga hormigas

dame_colonia_aux2::[[Float]]->Float->Float->Int->[[Float]]->[[Float]]->[Int]->[Hormiga]
dame_colonia_aux2 _ _ _ 0 _ _ _ = []
dame_colonia_aux2 costes alfa beta k ferom atract sems = (dame_hormiga2 costes alfa beta ferom atract iz [1]):(dame_colonia_aux2 costes alfa beta (k-1) ferom atract dr)
   where (iz,dr) = splitAt ((length ferom) -1)  sems

dame_hormiga2::[[Float]]->Float->Float->[[Float]]->[[Float]]->[Int]->[Int]->Hormiga --HAY QUE INICIALIZAR acum CUANDO LLAMEMOS A dame_hormiga CON [1]
dame_hormiga2 costes alfa beta ferom atract sems acum
   |(sig_paso==length ferom) = (H (acum++[sig_paso]) (coste_camino2 (acum++[sig_paso]) costes))
   |(sig_paso==0) = (H acum (1/0)) 
   | otherwise = dame_hormiga2 costes alfa beta ferom new_atract (tail sems) (acum++[sig_paso]) 
   where long_acum = length acum
         sig_paso = dame_paso2 alfa beta ferom atract (head sems) (acum !! (long_acum-1))
         new_atract = anula_col sig_paso atract

dame_paso2::Float->Float->[[Float]]->[[Float]]->Int->Int->Int
dame_paso2 alfa beta ferom atract sem paso_anterior 
    | probs == [] = 0
    | otherwise = dame_paso_aux ((0.0, -1):(scanl1 (sum_float_pos) (filter ((/=0).fst) (zip probs [1..])))) (fst (randomR (0, 1) (mkStdGen sem)))
    where probs = dame_probs ferom atract alfa beta paso_anterior
        

dame_probs::[[Float]]->[[Float]]->Float->Float->Int->[Float]
dame_probs ferom atract alfa beta paso_anterior
   |and (map (==0.0) (atract !! (paso_anterior-1))) = []
   |otherwise = map (/sumatorio_prob) fila_probs
   where sumatorio_prob = sum fila_probs
         fila_probs = zipWith (prod22 alfa beta) (ferom !! (paso_anterior-1)) (atract !! (paso_anterior-1))

coste_camino2::[Int]->[[Float]]->Float 
coste_camino2 (x:y:ys) xs = (matriz_elem x y xs)+(coste_camino2 (y:ys) xs)
coste_camino2 (y:[]) xs = 0  

--Para pasar un archivo de texto a matriz de costes----

procesaMatriz::String->[[Float]]
procesaMatriz xs = map procesaFila (lines xs)

procesaFila::String->[Float]
procesaFila xs = map procesaElem (words xs)

procesaElem::String->Float
procesaElem xs
   |xs == "INF" = 1/0
   |otherwise = (read::String->Float) xs

----------------------MAIN--------------------------------
leeInt::IO Int
leeInt = do c <- getLine
            return (read c)

leeIntEnRango::Int->Int->IO Int
leeIntEnRango men may = do putStr("Introduce un entero entre "++show men++" y "++show may++": ")
                           n <- leeInt
                           if(n>=men)&&(n<=may)
                              then return n
                              else do putStrLn("Error, repite")
                                      leeIntEnRango men may

leeListaTuplas::IO [(Float, Float)]
leeListaTuplas = do c <- getLine
                    return (read c)

leeFloat::IO Float
leeFloat = do c <- getLine
              return (read c)

main::IO()
main = do putStrLn "Bienvenido al optimizador por inteligencia de enjambres."
          putStrLn "Elija una de las tres opciones:"
          putStrLn "1- Optimizacion por enjambre de particulas (PSO) minimizacion."
          putStrLn "2- Optimizacion por colonia de hormigas (ACO)."
          putStrLn "3- Salir."
          opcion <- leeIntEnRango 1 3
          case opcion of
               1 -> do main_pso
               2 -> do main_aco
               3 -> putStr "Hasta la proxima"

main_pso::IO()
main_pso = do putStrLn "Elija una de las tres opciones:"
              putStrLn "1- Particulas sin salir del rectangulo de definicion."
              putStrLn "2- Particulas pueden salir del rectangulo de definicion."
              putStrLn "3- Volver atras."
              opcion <- leeIntEnRango 1 3
              case opcion of
                   1 -> do main_pso1
                   2 -> do main_pso2
                   3 -> do main

main_pso1::IO()
main_pso1 = do putStrLn "Elija una de las 4 funciones a optimizar:"
               putStrLn "1- f(x, y) = -2^{-(x-6)^2 -y^2}"
               putStrLn "2- f(x, y) = sinx + siny"
               putStrLn "3- f(x, y, z) = x^2+y^2+z^2"
               putStrLn "4- f(x, y) = (x^2+y^2)/(x^2+1)"
               opcion <- leeIntEnRango 1 4
               putStrLn "Introduzca el rectangulo de definicion:"
               rectangulo <- leeListaTuplas
               putStrLn "Introduzca el parametro de inercia:"
               w <- leeFloat
               putStrLn "Introduzca el peso de la mejor posicion local:"
               c1 <- leeFloat
               putStrLn "Introduzca el peso de la mejor posicion global:"
               c2 <- leeFloat
               putStrLn "Introduzca el numero de iteraciones:"
               n <- leeInt
               putStrLn "Introduzca el numero de particulas:"
               k <- leeInt
               putStrLn "Introduzca un entero como semilla:"
               s <- leeInt
               putStrLn "Desea guardar todo el proceso en un archivo de texto:"
               putStrLn "1- Si en formato 1."
               putStrLn "2- Si en formato 2."
               putStrLn "3- No."
               formato <- leeIntEnRango 1 3
               if (formato<3)
                  then do putStrLn "Dame el nombre del fichero de salida:"
                          nombreOut <- getLine
                          case opcion of
                               1 -> do writeFile nombreOut (optimizarPSO_datos2 g rectangulo w c1 c2 n k s formato)
                               2 -> do writeFile nombreOut (optimizarPSO_datos2 f rectangulo w c1 c2 n k s formato)
                               3 -> do writeFile nombreOut (optimizarPSO_datos2 h rectangulo w c1 c2 n k s formato)
                               4 -> do writeFile nombreOut (optimizarPSO_datos2 l rectangulo w c1 c2 n k s formato)
                  else do case opcion of
                               1 -> do putStrLn "La mejor solucion encontrada es:"
                                       putStrLn (show (optimizarPSO2 g rectangulo w c1 c2 n k s))
                               2 -> do putStrLn "La mejor solucion encontrada es:"
                                       putStrLn (show (optimizarPSO2 f rectangulo w c1 c2 n k s))
                               3 -> do putStrLn "La mejor solucion encontrada es:"
                                       putStrLn (show (optimizarPSO2 h rectangulo w c1 c2 n k s))
                               4 -> do putStrLn "La mejor solucion encontrada es:"
                                       putStrLn (show (optimizarPSO2 l rectangulo w c1 c2 n k s))
               main

main_pso2::IO()
main_pso2 = do putStrLn "Elija una de las 4 funciones a optimizar:"
               putStrLn "1- f(x, y) = -2^{-(x-6)^2 -y^2}"
               putStrLn "2- f(x, y) = sinx + siny"
               putStrLn "3- f(x, y, z) = x^2+y^2+z^2"
               putStrLn "4- f(x, y) = (x^2+y^2)/(x^2+1)"
               opcion <- leeIntEnRango 1 4
               putStrLn "Introduzca el rectangulo de definicion:"
               rectangulo <- leeListaTuplas
               putStrLn "Introduzca el parametro de inercia:"
               w <- leeFloat
               putStrLn "Introduzca el peso de la mejor posicion local:"
               c1 <- leeFloat
               putStrLn "Introduzca el peso de la mejor posicion global:"
               c2 <- leeFloat
               putStrLn "Introduzca el numero de iteraciones:"
               n <- leeInt
               putStrLn "Introduzca el numero de particulas:"
               k <- leeInt
               putStrLn "Introduzca un entero como semilla:"
               s <- leeInt
               putStrLn "Desea guardar todo el proceso en un archivo de texto:"
               putStrLn "1- Si en formato 1."
               putStrLn "2- Si en formato 2."
               putStrLn "3- No."
               formato <- leeIntEnRango 1 3
               if (formato<3)
                  then do putStrLn "Dame el nombre del fichero de salida:"
                          nombreOut <- getLine
                          case opcion of
                               1 -> do writeFile nombreOut (optimizarPSO_datos g rectangulo w c1 c2 n k s formato)
                               2 -> do writeFile nombreOut (optimizarPSO_datos f rectangulo w c1 c2 n k s formato)
                               3 -> do writeFile nombreOut (optimizarPSO_datos h rectangulo w c1 c2 n k s formato)
                               4 -> do writeFile nombreOut (optimizarPSO_datos l rectangulo w c1 c2 n k s formato)
                  else do case opcion of
                               1 -> do putStrLn "La mejor solucion encontrada es:"
                                       putStrLn (show (optimizarPSO f rectangulo w c1 c2 n k s))
                               2 -> do putStrLn "La mejor solucion encontrada es:"
                                       putStrLn (show (optimizarPSO f rectangulo w c1 c2 n k s))
                               3 -> do putStrLn "La mejor solucion encontrada es:"
                                       putStrLn (show (optimizarPSO f rectangulo w c1 c2 n k s))
                               4 -> do putStrLn "La mejor solucion encontrada es:"
                                       putStrLn (show (optimizarPSO f rectangulo w c1 c2 n k s))
               main
                          
               
main_aco::IO()
main_aco = do putStrLn "Elija una de las tres opciones:"
              putStrLn "1- Problema del viajante."
              putStrLn "2- Camino mas corto grafo general."
              putStrLn "3- Volver atras."
              opcion <- leeIntEnRango 1 3
              case opcion of
                   1 -> do main_aco1
                   2 -> do main_aco2
                   3 -> do main    

main_aco1::IO()     
main_aco1 = do putStrLn "Introduzca el nombre del fichero con la matriz de costes"
               fich_costes <- getLine
               costes_str <- readFile fich_costes
               let costes = procesaMatriz costes_str
               putStrLn "Introduzca el coeficiente de evaporacion"
               p <- leeFloat
               putStrLn "Introduzca el peso de las feromonas"
               alfa <- leeFloat
               putStrLn "Introduzca el grado de lejania"
               beta <- leeFloat
               putStrLn "Introduzca el grado de beneficio"
               q <- leeFloat
               putStrLn "Introduzca el multiplicador del beneficio al camino mas corto"
               benef <- leeFloat
               putStrLn "Introduzca el numero de hormigas"
               k <- leeInt
               putStrLn "Introduzca el numero de iteraciones"
               n <- leeInt
               putStrLn "Introduzca la semilla"
               s <- leeInt
               putStrLn "Desea guardar todo el proceso en un archivo de texto"
               putStrLn "1- Si en formato 1."
               putStrLn "2- Si en formato 2."
               putStrLn "3- No."
               formato <- leeIntEnRango 1 3
               if (formato<3)
                  then do putStrLn "Dame el nombre del fichero de salida:"
                          nombreOut <- getLine
                          writeFile nombreOut (optimizarACO_V_str costes p alfa beta q benef k n s formato)
                  else do putStrLn "La mejor solucion encontrada es:"
                          putStrLn (show (optimizarACO_V costes p alfa beta q benef k n s))
               main

main_aco2::IO()     
main_aco2 = do putStrLn "Introduzca el nombre del fichero con la matriz de costes"
               fich_costes <- getLine
               costes_str <- readFile fich_costes
               let costes = procesaMatriz costes_str
               putStrLn "Introduzca el coeficiente de evaporacion"
               p <- leeFloat
               putStrLn "Introduzca el peso de las feromonas"
               alfa <- leeFloat
               putStrLn "Introduzca el grado de lejania"
               beta <- leeFloat
               putStrLn "Introduzca el grado de beneficio"
               q <- leeFloat
               putStrLn "Introduzca el multiplicador del beneficio al camino mas corto"
               benef <- leeFloat
               putStrLn "Introduzca el numero de hormigas"
               k <- leeInt
               putStrLn "Introduzca el numero de iteraciones"
               n <- leeInt
               putStrLn "Introduzca la semilla"
               s <- leeInt
               putStrLn "La mejor solucion encontrada es:"
               putStrLn (show (optimizarACO_V2 costes p alfa beta q benef k n s))
               main



