import java.beans.FeatureDescriptor;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import feed.Article;
import feed.FeedParser;
import utils.Config;
import utils.FeedsData;
import utils.JSONParser;
import utils.UserInterface;
import utils.Entity;
import utils.EntityProcessor;
import utils.ComputeStats;
import utils.FileHelper;
import namedEntities.NamedEntity;
import namedEntities.heuristics.*;

public class App {

    public static void main(String[] args) {

        List<FeedsData> feedsDataArray = new ArrayList<>();
        try {
            feedsDataArray = JSONParser.parseJsonFeedsData("src/data/feeds.json");
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }

        UserInterface ui = new UserInterface();
        Config config = ui.handleInput(args);
        long startTime = System.currentTimeMillis(); // We measure the time it takes to process the feeds to later compare the performance of the Spark implementation
        run(config, feedsDataArray);
        long endTime = System.currentTimeMillis();
        long durationMillis = endTime - startTime;
        double durationSeconds = durationMillis / 1000.0;
        System.out.println("Process took " + durationSeconds + " seconds.");
    }

    private static void run(Config config, List<FeedsData> feedsDataArray) {
        if (config.getHelp()) {
            printHelp(feedsDataArray);
            System.exit(0);
        }

        if (feedsDataArray == null || feedsDataArray.isEmpty()) {
            System.out.println("No feeds data found");
            return;
        }

        List<Article> allArticles = getArticles(config, feedsDataArray);
        if (allArticles == null)
            return;

        if (config.getPrintFeed()) {
            printFeeds(allArticles);
        }

        if (config.getComputeNamedEntities()) {
            computeNamedEntities(config, allArticles);
        }

        if (config.getBigFeeds()) {
            FileHelper.articlesToFile(allArticles, "src/data", "bigFeeds.txt");
        }
    }

    private static List<Article> getArticles(Config config, List<FeedsData> feedsDataArray) {
        String feedKey = config.getFeedKey();

        if (feedKey == null) {
            return processAllFeeds(feedsDataArray);
        } else if (FeedsData.keyIsOnFeeds(feedKey, feedsDataArray)) {
            return processSpecificFeed(feedsDataArray, feedKey);
        } else {
            System.out.println("No such feed");
            System.out.println("Use '-h' to get help.");
            return null;
        }
    }

    private static List<Article> processAllFeeds(List<FeedsData> feedsDataArray) {
        List<Article> allArticles = new ArrayList<>();
        for (FeedsData feedData : feedsDataArray) {
            allArticles.addAll(FeedParser.processFeed(feedData));
        }
        return allArticles;
    }

    private static List<Article> processSpecificFeed(List<FeedsData> feedsDataArray, String feedKey) {
        for (FeedsData feedData : feedsDataArray) {
            if (feedData.getLabel().equals(feedKey)) {
                return FeedParser.processFeed(feedData);
            }
        }
        return new ArrayList<>();
    }

    private static void printFeeds(List<Article> allArticles) {
        System.out.println("Printing " + allArticles.size() + " feeds");
        // Print all the articles from the processed feed or feeds
        Article.printArticles(allArticles);
    }

    private static void computeNamedEntities(Config config, List<Article> allArticles) {
        List<Entity> entitiesFromJson = Entity.loadEntitiesFromJson();
        if (entitiesFromJson == null)
            return;

        System.out.println("Computing named entities using " + config.getHeuristic());
        NamedEntityHeuristic heuristic = NamedEntityHeuristic.selectHeuristic(config.getHeuristic());
        EntityProcessor processor = new EntityProcessor(allArticles, entitiesFromJson, heuristic);
        List<NamedEntity> nEList = processor.processEntities();
        printStats(config, nEList);
    }

    private static void printStats(Config config, List<NamedEntity> nEList) {
        ComputeStats stats = new ComputeStats(nEList);
        System.out.println("\nStats: ");
        System.out.println("-".repeat(80));

        String statsFormat = config.getStatsFormat();
        if (statsFormat == null || statsFormat.equals("cat")) {
            stats.printStats("Category", config.getHeuristic());
        } else if (statsFormat.equals("topic")) {
            stats.printStats("Topic", config.getHeuristic());
        } else {
            System.out.println("Invalid stats format");
        }
    }

    private static void printHelp(List<FeedsData> feedsDataArray) {
        System.out.println("Usage: make run ARGS=\"[OPTION]\"");
        System.out.println("Options:");
        System.out.println("  -h, --help: Show this help message and exit");
        System.out.println("  -f, --feed <feedKey>:                Fetch and process the feed with");
        System.out.println("                                       the specified key");
        System.out.println("                                       Available feed keys are: ");
        for (FeedsData feedData : feedsDataArray) {
            System.out.println("                                       " + feedData.getLabel());
        }
        System.out.println("  -ne, --named-entity <heuristicName>: Use the specified heuristic to extract");
        System.out.println("                                       named entities");
        System.out.println("                                       Available heuristic names are: ");
        System.out.println(
                "                                       CapitalizedWord: identifies named entities by looking for words");
        System.out.println(
                "                                                        where the first letter is capitalized and subsequent letters are lowercase.");
        System.out.println(
                "                                       MixedCaseWord: identifies named entities by looking for words where at least");
        System.out.println(
                "                                                      one letter is capitalized, but subsequent letters may be either lowercase or uppercase.");
        System.out.println(
                "                                       Acronym: identifies named entities by looking for sequences");
        System.out.println(
                "                                                of capitalized letters with a minimum length of two characters.");
        System.out.println("  -pf, --print-feed:                   Print the fetched feed");
        System.out.println("  -sf, --stats-format <format>:        Print the stats in the specified format");
        System.out.println("                                       Available formats are: ");
        System.out.println("                                       cat: Category-wise stats");
        System.out.println("                                       topic: Topic-wise stats");
    }

}
