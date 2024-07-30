package namedEntities.categories;

import java.util.List;

import namedEntities.NamedEntity;

public class Other extends NamedEntity {
    // Constructor
    public Other(String label, List<String> topics, int occurrences/* , String description */) {
        super(label, topics, occurrences);
    }
}