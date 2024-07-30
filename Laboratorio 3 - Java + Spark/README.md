Básicamente este repositorio tiene el ejemplo `JavaSparkEntityProcessor.java` que viene en la distribución de spark junto con el archivo `pom.xml` para poder compilarlo fácilmente. Esperamos que esto les sirva para poder empezar más fácilmente el lab3.

Pueden ver la consigna del laboratorio en https://docs.google.com/document/d/e/2PACX-1vQn5BpCPQ6jKMN-sz46261Qot82KbDZ1RUx8jNzAN4kBEAq_i97T3R6ZA0_yRA5elN66e-EArXQXuAh/pub

# Preparación del entorno
- [ ] Instalar [maven](https://maven.apache.org/guides/getting-started/maven-in-five-minutes.html) Es probable que sea un paquete de tu distribución (`$ sudo apt install maven` si estás en Ubuntu, Debian o derivados).
- [ ] Descargar [spark 3.5.1](https://www.apache.org/dyn/closer.lua/spark/spark-3.5.1/spark-3.5.1-bin-hadoop3.tgz) y descomprimirlo en el directorio `DIR`.
- [ ] Definir variable de entorno `export SPARK_HOME=<DIR>` (ahí `<DIR>` es el directorio donde descomprimieron spark).

# Qué hay en este repositorio


- `pom.xml` el archivo que describe el proyecto, indica las dependencias y la clase que se expone.
- `src/main/java/SparkEntityProcessor.java` el único módulo de nuestro proyecto.
- `data/quijote.txt` el Quijote en formato plano.
 
# Cómo compilarlo

Sea `APP_HOME` el directorio donde está este archivo `README.md`

```bash
$ cd $APP_HOME
$ mvn install
```
Eso descarga las dependencias que necesitamos y crea el directorio `target` donde queda el jar que nos interesa.
En mi caso:
```
$ ls target
classes  generated-sources  generated-test-sources  maven-archiver  maven-status  test-classes  SparkEntityProcessor-0.1.jar
```

# Cómo usarlo

En el directorio `$SPARK_HOME` pueden usar `bin/spark-submit`:
```bash
$ bin/spark-submit  --master local[2] $APP_HOME/target/SparkEntityProcessor-0.1.jar  $APP_HOME/data/quijote.txt
```

Si no quieren ver la información de spark pueden redirigir `stderr` a `/dev/null`:
```bash
$ bin/spark-submit  --master local[2] $APP_HOME/target/SparkEntityProcessor-0.1.jar  $APP_HOME/data/quijote.txt 2>/dev/null
```
