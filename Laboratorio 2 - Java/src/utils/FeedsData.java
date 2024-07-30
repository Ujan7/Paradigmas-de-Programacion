package utils;

import java.util.List;

public class FeedsData {
    private String label;
    private String url;
    private String type;

    public FeedsData(String label, String url, String type) {
        this.label = label;
        this.url = url;
        this.type = type;
    }

    public String getLabel() {
        return label;
    }

    public String getUrl() {
        return url;
    }

    public String getType() {
        return type;
    }

    public void print() {
        System.out.println("Feed: " + label);
        System.out.println("URL: " + url);
        System.out.println("Type: " + type);
    }

    // If the feedKey matchs with any label on our feeds, returns true
    public static boolean keyIsOnFeeds(String feedKey, List<FeedsData> feedsDataArray) {
        for (FeedsData feedData : feedsDataArray) {
            if (feedData.getLabel().equals(feedKey)) {
                return true;
            }
        }
        return false;
    }
}