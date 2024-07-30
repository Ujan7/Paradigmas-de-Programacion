
# Laboratorio de Programación Orientada a Objetos
---
- Integrantes: 
    - [x] 1. Eugenio Arcana
    - [x] 2. Facundo Coria
    - [x] 3. Juan Ignacio Diaz

El enunciado del laboratorio se encuentra en [este link](https://docs.google.com/document/d/1wLhuEOjhdLwgZ4rlW0AftgKD4QIPPx37Dzs--P1gIU4/edit#heading=h.xe9t6iq9fo58).

## Dependencias

Necesitan Java 17, tanto el JRE como el JDK. En Ubuntu, pueden instalarlo con:

```bash
apt install openjdk-17-jdk openjdk-17-jre
```

## Compilación y ejecución

- Para compilar el código ejecutamos `make`, lo cual crea todos los archivos compilados en el directorio `./bin`

- Para correr el código ejecutamos `make run ARGS="<flags>"` donde <flags> son las flags que corresponden a los args toma la función principal del software.

- `make clean` borra los archivos `.class` que se generan en la compilación.

## 1. Tareas
Pueden usar esta checklist para indicar el avance.

### Verificación de que pueden hacer las cosas.
- [x] Java 17 instalado. Deben poder compilar con `make` y correr con `make run` para obtener el mensaje de ayuda del programa.

### 1.1. Interfaz de usuario
- [x] Estructurar opciones
- [x] Construir el objeto de clase `Config`

### 1.2. FeedParser
- [x] `class Article`
    - [x] Atributos
    - [x] Constructor
    - [x] Método `print`
    - [x] _Accessors_
- [x] `parseXML`

### 1.3. Entidades nombradas
- [x] Pensar estructura y validarla con el docente
- [x] Implementarla
- [x] Extracción
    - [x] Implementación de heurísticas
- [x] Clasificación
    - [x] Por tópicos
    - [x] Por categorías
- Estadísticas
    - [x] Por tópicos
    - [x] Por categorías
    - [x] Impresión de estadísticas

### 1.4 Limpieza de código
- [x] Pasar un formateador de código
- [x] Revisar TODOs

## 2. Experiencia
### Sobre herencia
Al principio, trabajar con la herencia presentó ciertos desafíos, especialmente en la comprensión de cómo y cuándo usar las subclases. Por ejemplo, tuvimos que aprender a usar constructores sobrecargados para manejar diferentes inicializaciones de objetos. También entendimos la importancia del polimorfismo, que nos permitió tratar instancias de subclases como instancias de la clase base NamedEntity (EntityProcessor línea 55), simplificando así el manejo de diferentes tipos de entidades en nuestro código.
### Diseño
El diseño que utilizamos en un principio para NamedEntity parecía funcionar eficientemente, hasta que con la integración de la API y por la forma en que procesábamos las entidades observamos que carecía de eficiencia por la cantidad de requests que se hacían. Esto nos llevó a realizar un cambio en el diseño agregando un campo a la clase NamedEntity, que habría ahorrado mucho tiempo si lo hubiéramos pensado en un principio.

Basicamente, antes creabamos un objeto por cada entidad encontrada. Es decir, si encontrabamos "CGT" un total de 10 veces, se procesaba la entidad creando 10 objetos diferentes. Luego, por cada objeto, haciamos peticiones a la API para conseguir sus atributos. 

Para solucionar esto, agregamos un campo `occurrences` a NamedEntity, que nos permitió llevar un conteo de cuantas veces se encontraba una entidad. De esta forma, si encontrabamos "CGT" 10 veces, se creaba un solo objeto con 10 ocurrencias. Luego, al finalizar el procesamiento de todas las entidades, se hacía una sola petición a la API para conseguir los atributos de todas las entidades.

### Reflexión
Gracias a este trabajo, conseguimos una nueva visión sobre otro paradigma de programación muy poderoso y ampliamente utilizado.

Adquirimos una experiencia valiosa en la programación orientada a objetos, la gestión de proyectos y la resolución de problemas en el desarrollo de software. Pudimos ver la importancia de un diseño bien estructurado y la utilidad de la herencia en la organización del código. Además, cabe destacar que la toma de decisiones en este proyecto fue de suma importancia, y creemos que se tomaron decisiones acertadas en cuanto a la estructura del código y la implementación de las heurísticas.

Podemos concluir que, al finalizar este proyecto, nos sentimos cómodos con respecto a POO y con la capacidad de resolver problemas de manera más eficiente y estructurada. Además, sentimos que nos hemos familiarizado con el lenguaje y que estamos en condiciones de seguir aprendiendo y mejorando en el mismo.


## 3. Preguntas
1. Explicar brevemente la estructura de datos elegida para las entidades nombradas.
- La estructura se basa en una superclase llamada `NamedEntity`, que contiene los atributos label, topics y occurrences. Esta superclase es heredada por las clases `Location`, `Organization`, `Person` y `Other`. Cada una de estas categorías tiene al menos un atributo específico (excepto Other).
2. Explicar brevemente cómo se implementaron las heurísticas de extracción. 
- Se definió la superclase `NamedEntityHeuristic`, que contiene el método abstracto extractCandidates, el cual es implementado por las clases heredadas `CapitalizedWordHeuristic`, `MixedCaseWordHeuristic` y `AcronymHeuristic`. Ambas heurísticas de extracción (MixedCaseWordHeuristic y AcronymHeuristic) fueron implementadas utilizando expresiones regulares. La primera busca palabras con al menos una letra mayúscula (descartando palabras vacías como conectores y artículos), mientras que la segunda busca palabras en mayúsculas con al menos 2 caracteres.

## 4. Extras
- Computamos algunas estadísticas adicionales, como la ubicación más al norte, o la más fría. Así como la organización, persona y ubicación más repetidas.
- Los campos específicos de cada categoría están implementados de manera realista. Para Location, se obtienen las coordenadas y la temperatura a través de una API. Para `Person`, se muestran el nombre y apellido (extraídos directamente del diccionario) y para `Organization`, se muestra el nombre de la organización (también extraido del diccionario).