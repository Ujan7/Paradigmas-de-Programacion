package utils;

//This module is responsible for the configuration of our application.
public class Config {
    private boolean printFeed = false;
    private boolean computeNamedEntities = false;
    private String statsFormat;
    private String feedKey;
    private String heuristic;
    private boolean help = false;
    private boolean bigFeeds = false;

    public Config(boolean printFeed, boolean computeNamedEntities, String statsFormat, String feedKey, String heuristic,
            boolean help, boolean createBigFeeds) {
        this.printFeed = printFeed;
        this.computeNamedEntities = computeNamedEntities;
        this.statsFormat = statsFormat;
        this.feedKey = feedKey;
        this.heuristic = heuristic;
        this.help = help;
        this.bigFeeds = createBigFeeds;
    }

    public boolean getPrintFeed() {
        return printFeed;
    }

    public boolean getComputeNamedEntities() {
        return computeNamedEntities;
    }

    public String getStatsFormat() {
        return statsFormat;
    }

    public String getFeedKey() {
        return feedKey;
    }

    public String getHeuristic() {
        return heuristic;
    }

    public boolean getHelp() {
        return help;
    }

    public boolean getBigFeeds() {
        return bigFeeds;
    }
}
