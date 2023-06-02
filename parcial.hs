data Personaje = UnPersonaje{
    nombre :: String,
    dinero :: Int,
    felicidad :: Int
} deriving (Show, Eq)

---PERSONAJES---

homero = UnPersonaje "Homero" 15000 100
bart = UnPersonaje "Bart" 200 110
lisa = UnPersonaje "Lisa" 315 20
skinner = UnPersonaje "Skinner" 1000 5
srburns = UnPersonaje "SrBurns" 1000000 

listaFunciones = [irColegio, (comerDonas 20), (irTrabajar "bombero"), escuelaDirector, (mirarTele 3)]
listaInfinita = cycle [irColegio, (comerDonas 20), (irTrabajar "bombero"), escuelaDirector, (mirarTele 3)]

---FUNCIONES---

irColegio :: Personaje -> Personaje
irColegio personaje
    | personaje == lisa = personaje {felicidad = max 0 (felicidad personaje + 20)}
    | otherwise = personaje {felicidad = max 0 (felicidad personaje - 20)}

comerDonas :: Int -> Personaje -> Personaje
comerDonas cantidad personaje = personaje {dinero = dinero personaje - 10*cantidad, felicidad = max 0 (felicidad personaje + 10*cantidad) }

irTrabajar :: String -> Personaje -> Personaje
irTrabajar trabajo personaje = personaje {dinero = dinero personaje + length trabajo}

escuelaDirector :: Personaje -> Personaje
escuelaDirector personaje = personaje {dinero = dinero personaje + length "escuela elemental", felicidad = max 0 (felicidad personaje - 20)}

mirarTele :: Int -> Personaje -> Personaje --si miras un hora de television te su la felicidad 1 pero te baja $10
mirarTele horas personaje = personaje {dinero = dinero personaje - 10*horas, felicidad = felicidad personaje + horas}

actividadDecisiva :: Int -> String -> (Personaje -> Personaje) -> Personaje -> String
actividadDecisiva cifra logro f personaje
    | logro == "millonario" && dinero (f personaje) > 1000000 = "actividad decisiva"
    | logro == "alegrarse" && felicidad (f personaje) > cifra = "actividad decisiva"
    | logro == "krosti" && dinero (f personaje) >= 10 = "actividad decisiva"
    | logro == "depresion" && felicidad (f personaje) == 0 = "actividad decisiva"
    | otherwise = "actividad no decisiva"

listaActividades :: [Personaje -> Personaje] -> String -> Personaje ->  Personaje
listaActividades listaFunciones logro personaje 
    | (actividadDecisiva 200) logro (head listaFunciones) personaje == "actividad decisiva" = (head listaFunciones) personaje
    | otherwise = listaActividades (drop 1 listaFunciones) logro personaje
