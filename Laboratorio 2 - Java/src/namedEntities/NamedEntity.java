package namedEntities;

import java.util.List;

public class NamedEntity {
    private List<String> topics;
    private String label;
    private int occurrences;

    // Constructor
    public NamedEntity(String label, List<String> topics, int occurrences) {
        this.label = label;
        this.topics = topics;
        this.occurrences = occurrences;
    }

    // Getters
    public List<String> getTopics() {
        return topics;
    }

    public String getLabel() {
        return label;
    }

    public int getOccurrences() {
        return occurrences;
    }

    // Setters
    public void setTopics(List<String> topics) {
        this.topics = topics;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public void setOccurrences(int occurrences) {
        this.occurrences = occurrences;
    }
}
