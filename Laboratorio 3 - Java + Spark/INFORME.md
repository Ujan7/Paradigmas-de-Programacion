# Laboratorio 3: Computación Distribuida con Apache Spark

- Integrantes: 
    - [x] 1. Eugenio Arcana
    - [x] 2. Facundo Coria
    - [x] 3. Juan Ignacio Diaz


## Compilación y ejecución


## 1. Introducción
En este proyecto hemos extendido el lector de feeds automático implementado en el laboratorio anterior para que soporte el procesamiento de grandes volúmenes de datos, también conocido como "big data". Con el objetivo de manejar varios gigabytes de datos al calcular entidades nombradas, hemos utilizado Apache Spark, uno de los frameworks de big data y computación distribuida más utilizados actualmente. Este laboratorio se ha diseñado más como un trabajo de investigación que de desarrollo, requiriendo una cantidad mínima de codificación en comparación con los laboratorios anteriores.

El objetivo de este laboratorio fue adquirir una experiencia práctica con Spark, computando las entidades nombradas en un gran archivo de texto de forma distribuida. Para lograr esto, investigamos sobre la arquitectura de Spark y su API para Java, y lo integramos en nuestra aplicación ya existente.


## 2. Cambios introducidos en el laboratorio 2


## 3. SparkEntityProcessor

Implementa todo el procesamiento utilizando Apache Spark, aprovechando su capacidad para distribuir el trabajo en múltiples nodos y procesar grandes volúmenes de datos de manera eficiente. En este módulo es clave el uso de ```JavaRDD``` (Resilient Distributed Dataset), una abstracción fundamental en Apache Spark que representa una colección distribuida de elementos que puede ser procesada en paralelo. El flujo del programa es el siguiente:

1. Lee un archivo de texto que potencialmente puede ser muy grande (varios gigabytes) y procesa cada línea del archivo.
``` Java
   JavaRDD<String> lines = sc.textFile(filePath);
```
2. Utiliza una heurística especificada por el usuario para extraer candidatos que puedan ser posibles entidades nombradas de cada línea del texto.
``` Java
        JavaPairRDD<String, Integer> candidatePairs = lines.flatMapToPair(line -> {
            List<Tuple2<String, Integer>> cList = new ArrayList<>();
            selectedHeuristic.extractCandidates(line).forEach(candidate -> cList.add(new Tuple2<>(candidate, 1)));
            return cList.iterator();
        });
```
3. Cuenta cuántas veces aparece cada candidata en el archivo de texto.
``` Java
    JavaPairRDD<String, Integer> candidateCounts = candidatePairs.reduceByKey(Integer::sum);
```
4. Procesa cada entidad candidata utilizando un EntityProcessor para determinar las entidades nombradas finales.
``` Java
    EntityProcessor processor = new EntityProcessor(null, entitiesFromJson, selectedHeuristic);
    JavaRDD<NamedEntity> namedEntities = candidateCounts.map(candidate -> {
        NamedEntity nEntity = processor.computeSingleEntity(candidate._1(), candidate._2());
        return nEntity;
    });
```
5. Genera estadísticas sobre las entidades nombradas utilizando la heurística y criterio especificados y las muestra al usuario.
``` Java
    List<NamedEntity> output = namedEntities.collect();
    ComputeStats stats = new ComputeStats(output);
    stats.printStats(criterion, heuristicName);
```
En este módulo, se utilizan diversas funciones de Apache Spark para realizar el procesamiento distribuido de datos. Las funciones map, ```flatMap```, ```reduceByKey``` y ```collect``` son esenciales para transformar y analizar grandes volúmenes de datos de manera eficiente. map y flatMap se emplean para transformar cada línea del archivo de entrada y extraer entidades candidatas utilizando una heurística. ```reduceByKey``` se usa para contar la frecuencia de cada candidato, combinando los resultados de manera eficiente a través de los nodos del clúster. Finalmente, ```collect``` reúne los resultados distribuidos en una lista para su procesamiento final y generación de estadísticas, permitiendo así manejar grandes datos con rapidez y eficacia.

## 4. Evaluación del desempeño
Se evaluó el desempeño de SparEntityProcessor usando el archivo wiki_dump_parcial.txt otorgado por la cátedra. El objetivo era medir cómo varía el tiempo de procesamiento al aumentar el número de nodos (trabajadores) en el clúster Spark. Los resultados obtenidos se presentan en la siguiente tabla:

|    WORKERS   |     TIME     |
|--------------|--------------|
|       1      | 107,582388 s |
|       2      | 57,936522 s  |
|       4      | 34,830205 s  |
|       6      | 26,617750 s  |

### Análisis de los resultados:

1. Los resultados muestran una clara disminución en el tiempo de procesamiento a medida que se incrementa el numero de trabajadores, lo que indica que `SparkEntityPocessor` escala de manera eficiente con el numero de nodos en el cluster.
2. Con un solo trabajador, vemos que el tiempo de procesamiento es de ~108 segundos. Al duplicar el numero de trabajadores, el tiempo se reduce a ~58 segundos, mostrando casi una reducción del 50%. Esto demuestra una alta eficiencia del paralelismo en los primeros aumentos de trabajadores.
3. La reducción del tiempo de procesamiento de 4 a 6 trabajadores no es tan pronunciada como de 1 a 2 o de 2 a 4. Esto es lo esperado en sistemas paralelos, ya que hay una sobrecarga asociada con la comunicación y la coordinación entre más nodos.
Esto ultimo también lo pudimos ver al computar las entidades de bigFeeds.txt. Lo que observamos fue que el tiempo de procesamiento al usar 2 núcleos es menor que al usar todos los núcleos.


## 5. Punto extra

**Nota:** este análisis se realizó en base al archivo `feedsv2.json`.

### Comparativa entre la velocidad de respuesta del laboratorio 2 vs la version distribuida (laboratorio 3)
Para obtener el tiempo de procesamiento del laboratorio 2 hemos definido las variables `startTime` y `endTime`, en la guarda que computa las entidades nombradas(`computeNamedEntities`), quedando de la siguiente manera:
``` Java
    if (config.getComputeNamedEntities()) {
        long startComputeTime = System.currentTimeMillis();
        computeNamedEntities(config, allArticles);
        long endComputeTime = System.currentTimeMillis();
        long computeDurationMillis = endComputeTime - startComputeTime;
        double computeDurationSeconds = computeDurationMillis / 1000.0;
        System.out.println("El método computeNamedEntities tardó " + computeDurationSeconds + " segundos en ejecutarse.");
    }
```

Luego de correr el proyecto 2 con estas modificaciones obtuvimos tiempos con una media de 8 segundos, computando entidades y estadísticas de todos los feeds, e imprimiendo las estadísticas por categoría usando CapitalizedWord, utilizando el siguiente comando:
`make run ARGS="-ne CapitalizedWord -sf cat"`

Mientras que por otro lado, corriendo la version distribuida del proyecto, obtuvimos una media de 7 segundos.

De estos resultados podemos concluir que para archivos pequeños se obtienen resultados muy similares, esto tiene que ver con la sobrecarga ya mencionada en el punto 3 de Análisis de los resultados. 

## 6. Experiencia y Conclusión

La implementación de este laboratorio nos ha permitido adquirir una valiosa experiencia práctica en el manejo de grandes volúmenes de datos utilizando Apache Spark. A lo largo del proyecto, enfrentamos varios desafíos que nos ayudaron a profundizar en el conocimiento de las arquitecturas distribuidas y las técnicas de procesamiento de datos en paralelo. A continuación, se detallan las conclusiones más relevantes de nuestra experiencia:

### Adaptación del Código Existente
Si bien fueron necesarias modificaciones en la implementación del laboratorio 2, consideramos que la estructura ya implementada tenia buenas bases y sin mayores complicaciones pudimos adaptar el código para trabajar de manera distribuida con la arquitectura de Spark. Para esta etapa fue de vital importancia la investigación sobre el framework a utilizar, como por ejemplo el tipo JavaRDD, funciones `map`, `reduce` y `collect`.

### Manejo de la Sobrecarga de Comunicación
Durante las pruebas, notamos que el incremento en el número de trabajadores no siempre se tradujo en una reducción proporcional del tiempo de procesamiento. Esto se debió a la sobrecarga de comunicación y coordinación entre los nodos del clúster.

### Optimización del Proceso de Cómputo
La necesidad de optimizar los métodos para que trabajen con elementos individuales en lugar de listas completas fue crucial para aprovechar las capacidades de paralelismo de Spark. Este enfoque nos permitió distribuir el procesamiento de entidades nombradas de manera eficiente a través de los nodos del clúster.