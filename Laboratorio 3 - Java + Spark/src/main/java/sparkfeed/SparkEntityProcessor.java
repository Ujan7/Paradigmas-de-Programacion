package sparkfeed;

import scala.Tuple2;
import utils.ComputeStats;
import utils.Entity;
import utils.EntityProcessor;

import org.apache.spark.api.java.JavaPairRDD;
import org.apache.spark.api.java.JavaRDD;
import org.apache.spark.api.java.JavaSparkContext;
import org.apache.spark.sql.SparkSession;

import namedEntities.NamedEntity;
import namedEntities.heuristics.NamedEntityHeuristic;

import java.util.ArrayList;
import java.util.List;

public class SparkEntityProcessor {

    public static void main(String[] args) throws Exception {

        // Verify if an argument (file path) is provided
        if (args.length < 3) {
            System.err.println("Usage: SparkEntityProcessor <file> <heuristic> <criterion>");
            System.exit(1);
        }

        String filePath = args[0];
        String heuristicName = args[1];
        String criterion = args[2];

        // Create a Spark session (SparkSession)
        SparkSession spark = SparkSession.builder()
                .appName("SparkEntityProcessor") // Set Spark application name
                .getOrCreate();

        JavaSparkContext sc = new JavaSparkContext(spark.sparkContext());

        // Read the text file and create an RDD of lines
        JavaRDD<String> lines = sc.textFile(filePath);

        List<Entity> entitiesFromJson = Entity.loadEntitiesFromJson();
        NamedEntityHeuristic selectedHeuristic = NamedEntityHeuristic.selectHeuristic(heuristicName);

        JavaPairRDD<String, Integer> candidatePairs = lines.flatMapToPair(line -> {
            List<Tuple2<String, Integer>> cList = new ArrayList<>();
            selectedHeuristic.extractCandidates(line).forEach(candidate -> cList.add(new Tuple2<>(candidate, 1)));
            return cList.iterator();
        });

        // Reducir por clave (candidato) para contar las ocurrencias
        JavaPairRDD<String, Integer> candidateCounts = candidatePairs.reduceByKey(Integer::sum);

        EntityProcessor processor = new EntityProcessor(null, entitiesFromJson, selectedHeuristic);

        JavaRDD<NamedEntity> namedEntities = candidateCounts.map(candidate -> {
            NamedEntity nEntity = processor.computeSingleEntity(candidate._1(), candidate._2());
            return nEntity;
        });

        List<NamedEntity> output = namedEntities.collect();

        ComputeStats stats = new ComputeStats(output);
        stats.printStats(criterion, heuristicName);

        // Stop the Spark session
        spark.stop();
        sc.close();
    }
}