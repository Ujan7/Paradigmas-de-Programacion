package utils;

import java.util.List;

public class Entity {
    String label;
    String category;
    List<String> topics;
    List<String> keywords;

    public Entity(String label, String category, List<String> topics, List<String> keywords) {
        this.label = label;
        this.category = category;
        this.topics = topics;
        this.keywords = keywords;

    }

    // Getters
    public String getLabel() {
        return label;
    }

    public String getCategory() {
        return category;
    }

    public List<String> getTopics() {
        return topics;
    }

    public List<String> getKeywords() {
        return keywords;
    }

    // Setters
    public void setLabel(String label) {
        this.label = label;
    }

    public void setCategory(String category) {
        this.category = category;
    }

    public void setTopics(List<String> topics) {
        this.topics = topics;
    }

    public void setKeywords(List<String> keywords) {
        this.keywords = keywords;
    }
}
