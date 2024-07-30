package namedEntities.categories;

import java.util.List;

import namedEntities.NamedEntity;

public class Organization extends NamedEntity {
    private String name;

    // Constructor
    public Organization(String label, List<String> topics, int occurrences, String name) {
        super(label, topics, occurrences);
        this.name = name;
    }

    // Getters
    public String getName() {
        return name;
    }

    // Setters
    public void setName(String name) {
        this.name = name;
    }
}