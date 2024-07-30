# Laboratorio de Funcional
---
- [Enunciado del proyecto](https://tinyurl.com/funcional-2024-famaf)
- **Integrantes**: Facundo Coria, Juan Ignacio Diaz y Eugenio Arcana.

## Organización y desarrollo del proyecto
Como programadores, sabemos que es de vital importancia trabajar organizadamente y con objetivos puntuales. Pero a la hora de hacerlo en equipo, esto se vuelve aún más importante. Por eso, hemos decidido dividir el proyecto en sprints, para poder tener objetivos claros y alcanzables en un tiempo determinado. 

Un sprint es un periodo de tiempo en el que se desarrollan y entregan funcionalidades del proyecto. En nuestro caso, cada sprint tendrá una duración de una semana. Al finalizar cada sprint, se evaluará el trabajo realizado y se definirán las tareas a realizar en el siguiente sprint.

Aunque esto no es para nada sencillo, creemos que atacar el proyecto de esta manera, nos permitirá tener un mejor control del desarrollo del proyecto y una mejor comunicación entre los integrantes del equipo. 

Finalmente, para desarollar, utilizaremos la herramienta de Bitbucket, la cual nos permite crear un workspace y lograr un control de versiones del proyecto.

A continuación, se detallan las tareas a realizar en cada sprint y el estado de las mismas (a modo de Backlog).

#### Sprint 1
- [x] Crear un workspace en Bitbucket.
- [x] Initial commit y push del esqueleto del proyecto.
- [x] Dividir el proyecto en tareas.
- [x] Asignar tareas a cada integrante.
- [x] Evaluar y definir el segundo sprint.

#### Sprint 2
- [x] Implementar el modulo Dibujo.
- [x] Implementar el modulo Pred.
- [x] Implementar el modulo Interp.
- [x] Evaluar y definir el tercer sprint.

#### Sprint 3
- [x] Testing de los módulos ya implementados.
- [x] Dibujo Grilla.
- [x] Dibujo Escher.
- [x] Evaluar y definir el cuarto sprint.

#### Sprint 4
- [x] Merge request y revisión de código.
- [x] Documentacion codigo si se considera necesario.
- [x] Documentacion general del proyecto.
- [x] Release de la versión 1.0. 

Cabe destacar, que durante los dos primeros sprints se trabajo de forma paralela sobre los mismos modulos. Es decir, a cada integrante se le asignaron ciertas tareas pero no del todo independientes (es decir, se dio el caso de que dos integrantes del equipo desarollen en el mismo modulo). En cambio, en el tercer sprint cada desarollador tuvo la responsabilidad de implementar un modulo por si mismo.

**Importante**: las estimaciones no fueron perfectas y tuvimos complicaciones en la implementación de los módulos. Pero sentimos que la comunicación y el trabajo en equipo fue muy bueno, lo que nos permitió superar los obstáculos y cumplir con los objetivos planteados (no siempre cumpliendo el plazo establecido de una semana por cada sprint).

---

## Compilar el proyecto

* Mostrar por pantalla un dibujo en particular:
```bash
cabal run dibujos <Modulo.hs>
```

* Listar dibujos disponibles y consultar cual mostrar:
```bash
cabal run dibujos -- -l
```

* Correr tests:
```bash
ghc -package HUnit -i./src test/<Modulo.hs>
./test/<Modulo>
```
## Preguntas
1. ¿Por qué están separadas las funcionalidades en los módulos indicados? Explicar detalladamente la responsabilidad de cada módulo.

La idea de tener un software dividido en módulos se basa en el concepto de modularidad, que es una práctica de diseño de software donde el sistema se construye a partir de componentes o módulos independientes, cada uno con una función específica y bien definida. Esto con el objetivo de:

* Facilitar la comprensión del código.
* Mejora la mantenibilidad.
* Fomenta la reutilización.
* Permite la escalabilidad.

**Modulos principales y sus funciones**

* **Dibujo.hs**: contiene la sintaxis de nuestro lenguaje, funciones constructuras de figuras, funciones de manipulación de figuras y lo mas importante, el tipo Dibujo. Aqui, no se exportan constructures, sino funciones que nos permiten construir figuras por medio de los mismos.
* **Pred.hs**: contiene funciones que nos permiten definir predicados sobre figuras.
* **Interp.hs**: es el modulo encargado de interpretar las figuras y mostralas en pantalla. Basicamente, recibe un Dibujo y vectores, y los transforma en un FloatingPic para que Gloss (libreria que utilizamos) pueda mostrarlo por pantalla.
* **Main.hs**: modulo principal del proyecto. Aqui se encuentran las funciones necesarias para que el usuario interactue con el programa y pueda elegir que dibujo mostrar.
* **TestDibujo.hs y TestPred.hs**: modulo encargado de realizar los tests de los modulos anteriores.
* **Escher.hs y MiGrilla.hs**: dibujos implementados por nosotros, en los cuales tuvimos que tomar la decision de que tipo de datos utilizar para representarlos.
  * **Escher**: utilizamos enteros para representar las figuras, ya que nos parecio mas sencillo y escalable.
  * **MiGrilla**: utilizamos duplas de enteros.
  
2. ¿Por qué las figuras básicas no están incluidas en la definición del lenguaje, y en vez de eso, es un parámetro del tipo?
   
Las dos ventajas principales que recalcamos son:

* **Escalabilidad**: nos permite agregar figuras de forma sencilla, sin la necesidad de modificar el tipo Dibujo. Ademas, no existe una cota de figuras posibles, simplemente podemos agregar las que necesitemos o queramos.
* **Independencia del tipo de dato**: nos permite interpretar figuras de forma mas abstracta, sin la necesidad de definir cada una de las figuras posibles sobre el tipo Dibujo.

3. ¿Qué ventaja tiene utilizar una función de fold sobre hacer pattern-matching directo?

La funcion ```fold``` puede ser compleja de entender, pero una vez que se comprende, es una herramienta muy poderosa. La ventaja de utilizar ```fold``` sobre pattern-matching directo es que nos permite abstraer la recursión y la acumulación de valores en una sola función. Por medio de ```fold```, podemos aplicar cualquier funcion recursiva a nuestro tipo de datos, sin la necesidad de definir una funcion para cada caso. Por ejemplo, si tenemos:

```haskell
data Tree a = Empty | Node a (Tree a) (Tree a)
```

Y queremos definir dos funciones recursivas, de la forma:

```haskell
sumTree :: Tree Int -> Int
sumTree Empty = 0
sumTree (Node x l r) = x + sumTree l + sumTree r

prodTree :: Tree Int -> Int
prodTree Empty = 1
prodTree (Node x l r) = x * prodTree l * prodTree r
```

Con ```fold``` nos abstraemos la recursion o simplemente nos ahorramos definir las funciones recursivas que debamos implementar, utilizando funciones que sigan el patron recursivo de las antes mencionadas (identidad y acumulador):

```haskell
foldTree :: (a -> b) -> (b -> a -> b -> b) -> Tree a -> b
foldTree f g (UnNodo a) = f a
foldTree f g (Nodo izq a der) = g (foldTree f g izq) a (foldTree f g der)

sumTree :: Tree Int -> Int
sumTree = foldTree id (\l x r -> l + x + r)
```

4. ¿Cuál es la diferencia entre los predicados definidos en Pred.hs y los tests?
   
* Los ```predicados``` definidos en Pred.hs son funciones que reciben un Dibujo y devuelven un Bool. Estas funciones nos permiten definir propiedades sobre los dibujos, por ejemplo si tenemos un ``Dibujo`` conformado por ``Figuras`` basicas ``Int``, un predicado valido seria chequear si esas figuras son pares. No tienen un grado de correctitud acerca de un dibujo, es decir, no indican si un dibujo esta mal formado o si es correcto en terminos del tipo.
* Los ```tests``` son funciones que reciben un Dibujo y devuelven un Bool. Estas funciones nos permiten verificar si un dibujo esta bien formado o no. Es decir, si cumple con las reglas de nuestro lenguaje. Si reflejan la correctitud de nuestra implementación, ya que exponen posibles fallas o bugs en nuestro software.

