package utils;

import java.util.List;
import java.util.ArrayList;
import java.util.Collections;

import feed.Article;
import namedEntities.categories.Location;
import namedEntities.categories.Organization;
import namedEntities.categories.Other;
import namedEntities.categories.Person;
import namedEntities.NamedEntity;
import namedEntities.heuristics.NamedEntityHeuristic;
import org.json.JSONObject;
import java.io.Serializable;

public class EntityProcessor implements Serializable {
    private List<Article> allArticles;
    private List<Entity> entitiesFromDict;
    private NamedEntityHeuristic heuristic;

    public EntityProcessor(List<Article> allArticles, List<Entity> entitiesFromDict, NamedEntityHeuristic heuristic) {
        this.allArticles = allArticles;
        this.entitiesFromDict = entitiesFromDict;
        this.heuristic = heuristic;
    }

    public List<NamedEntity> processEntities() {
        List<NamedEntity> computedEntities = new ArrayList<>();
        for (Article article : allArticles) {
            processSingleArticle(article, heuristic, computedEntities);
        }
        return computedEntities;
    }

    public void processSingleArticle(Article article, NamedEntityHeuristic heuristic,
            List<NamedEntity> computedEntities) {
        String articleTitle = article.getTitle();
        String articleDescription = article.getDescription();

        List<String> candidatesFromTitle = heuristic.extractCandidates(articleTitle);
        List<String> candidatesFromDesc = heuristic.extractCandidates(articleDescription);

        addToComputedEntities(candidatesFromTitle, computedEntities);
        addToComputedEntities(candidatesFromDesc, computedEntities);
    }

    public NamedEntity computeSingleEntity(String candidate, Integer occurrences) {
        NamedEntity namedEntity;
        for (Entity entityFromDict : entitiesFromDict) {

            if (candidate.equals(entityFromDict.getLabel()) || entityFromDict.getKeywords().contains(candidate)) {

                String category = entityFromDict.getCategory();
                List<String> topics = entityFromDict.getTopics();
                namedEntity = createNamedEntity(category, entityFromDict.getLabel(), topics, occurrences);
                return namedEntity;
            }
        }
        namedEntity = createNamedEntity("OTHER", candidate, Collections.singletonList("OTHER"), occurrences);
        return namedEntity;
    }

    public void addToComputedEntities(List<String> candidates, List<NamedEntity> computedEntities) {
        for (String candidate : candidates) {

            boolean candidateIsOnDict = false;
            for (Entity entityFromDict : entitiesFromDict) {

                if (candidate.equals(entityFromDict.getLabel()) || entityFromDict.getKeywords().contains(candidate)) {
                    candidateIsOnDict = true;

                    if (!entityIsComputed(entityFromDict.getLabel(), computedEntities)) { // if isComputed this method increments nEntity
                                                                        // occurrences
                        String category = entityFromDict.getCategory();
                        List<String> topics = entityFromDict.getTopics();
                        NamedEntity namedEntity = createNamedEntity(category, entityFromDict.getLabel(), topics, 1);
                        computedEntities.add(namedEntity);
                        break;
                    }
                }
            }
            if (!candidateIsOnDict && !entityIsComputed(candidate, computedEntities)) {
                NamedEntity namedEntity = createNamedEntity("OTHER", candidate, Collections.singletonList("OTHER"), 1);
                computedEntities.add(namedEntity);
            }
        }
    }

    public boolean entityIsComputed(String newNEntityLabel, List<NamedEntity> computedEntities) {
        boolean isComputed = false;

        for (NamedEntity computedEntity : computedEntities) {
            if (computedEntity.getLabel().equals(newNEntityLabel)) {
                // entity is computed -> Increment occurrences
                computedEntity.setOccurrences(computedEntity.getOccurrences() + 1);
                isComputed = true;
                break;
            }
        }
        return isComputed;
    }

    public static String getNECategory(NamedEntity nEntity) {
        if (nEntity instanceof Person) {
            return "PERSON";
        } else if (nEntity instanceof Organization) {
            return "ORGANIZATION";
        } else if (nEntity instanceof Location) {
            return "LOCATION";
        } else if (nEntity instanceof Other) {
            return "OTHER";
        } else {
            return "UNKNOWN";
        }
    }

    private NamedEntity createNamedEntity(String category, String label, List<String> topics, Integer occurrences) {
        switch (category) {
            case "PERSON":
                String name = extractName(label);
                String lastName = extractLastName(label);
                return new Person(label, topics, occurrences, name, lastName);
            case "ORGANIZATION":
                String orgName = extractOrgName(label);
                return new Organization(label, topics, occurrences, orgName);
            case "LOCATION":
                String normalizedLabel = label.replaceAll(" ", "-");
                ApiInterface locInfo = new ApiInterface(normalizedLabel);
                JSONObject jsonData = locInfo.getInfo();

                if (jsonData == null) {
                    return new Location(label, topics, occurrences);
                }

                Location location = JSONParser.parseLocInfo(jsonData, label, topics);
                return location;
            case "OTHER":
                return new Other(label, topics, occurrences);
            default:
                return new NamedEntity(label, topics, occurrences);
        }
    }

    private String extractName(String label) {
        for (Entity entityFromDict : entitiesFromDict) {
            if (entityFromDict.getLabel().equals(label)) {
                String[] fullNameParts = entityFromDict.getKeywords().get(1).split(" ");
                return fullNameParts[0]; // Assuming the first part is the first name
            }
        }
        return "Unknown";
    }

    private String extractLastName(String label) {
        for (Entity entityFromDict : entitiesFromDict) {
            if (entityFromDict.getLabel().equals(label)) {
                String[] fullNameParts = entityFromDict.getKeywords().get(1).split(" ");
                return fullNameParts[fullNameParts.length - 1]; // Assuming the last part is the last name
            }
        }
        return "Unknown";
    }

    private String extractOrgName(String label) {
        for (Entity entityFromDict : entitiesFromDict) {
            if (entityFromDict.getLabel().equals(label)) {
                return entityFromDict.getKeywords().get(1);
            }
        }
        return "Unknown";
    }
}
