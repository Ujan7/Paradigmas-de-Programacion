package feed;

import java.io.Serializable;
import java.util.List;

public class Article implements Serializable {
    private String title;
    private String description;
    private String pubDate;
    private String link;

    // Constructor
    public Article(String title, String description, String pubDate, String link) {
        this.title = title;
        this.description = description;
        this.pubDate = pubDate;
        this.link = link;
    }
    
    // Prints the article in a "pretty way"
    public void printArticle() {
        System.out.println("Title: " + title);
        System.out.println("Description: " + description);
        System.out.println("Publication Date: " + pubDate);
        System.out.println("Link: " + link);
        System.out.println("************************************");
    }

    // Getters
    public String getTitle() {
        return title;
    }

    public String getDescription() {
        return description;
    }

    // Print a list of articles
    public static void printArticles(List<Article> articles) {
        for (Article article : articles) {
            article.printArticle();
        }
    }
}