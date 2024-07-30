package namedEntities.categories;

import java.util.List;

import namedEntities.NamedEntity;

public class Person extends NamedEntity {
    private String name;
    private String lastName;

    // Constructor
    public Person(String label, List<String> topics, int occurrences, String name, String lastName) {
        super(label, topics, occurrences);
        this.name = name;
        this.lastName = lastName;
    }

    // Getters
    public String getName() {
        return name;
    }

    public String getLastName() {
        return lastName;
    }

    // Setters
    public void setName(String name) {
        this.name = name;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }
}
