package utils;

import namedEntities.NamedEntity;
import namedEntities.categories.Location;
import namedEntities.categories.Person;
import namedEntities.categories.Organization;

import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.stream.Collectors;

public class ComputeStats {
    private List<NamedEntity> nEntities;

    public ComputeStats(List<NamedEntity> nEntities) {
        this.nEntities = nEntities;
    }

    /* *********************************************************************** */

    public void addNamedEntityToMap(NamedEntity nEntity, String criterion, Map<String, List<NamedEntity>> nEntitiesMap) {
        nEntitiesMap.computeIfAbsent(criterion, k -> new ArrayList<>()).add(nEntity);
    }

    /* ************************ GETTER ************************* */

    Map<String, Map<String, List<NamedEntity>>> getNamedEntitiesMap(String criterion) {
        Map<String, List<NamedEntity>> personsMap = new HashMap<>();
        Map<String, List<NamedEntity>> organizationsMap = new HashMap<>();
        Map<String, List<NamedEntity>> locationsMap = new HashMap<>();
        Map<String, List<NamedEntity>> othersMap = new HashMap<>();

        for (NamedEntity nEntity : nEntities) {
            List<String> keys = criterion.equals("Topic") ? nEntity.getTopics() : List.of(EntityProcessor.getNECategory(nEntity));
            for (String key : keys) {
                if (nEntity instanceof Person) {
                    addNamedEntityToMap(nEntity, key, personsMap);
                } else if (nEntity instanceof Organization) {
                    addNamedEntityToMap(nEntity, key, organizationsMap);
                } else if (nEntity instanceof Location) {
                    addNamedEntityToMap(nEntity, key, locationsMap);
                } else {
                    addNamedEntityToMap(nEntity, key, othersMap);
                }
            }
        }

        Map<String, Map<String, List<NamedEntity>>> combinedMap = new HashMap<>();
        combinedMap.put("Persons", personsMap);
        combinedMap.put("Organizations", organizationsMap);
        combinedMap.put("Locations", locationsMap);
        combinedMap.put("Others", othersMap);

        return combinedMap;
    }

    /* ************************ HELPER METHODS ************************ */

    private NamedEntity getMostFrequentEntity(List<NamedEntity> entities) {
        return entities.stream()
                .max((e1, e2) -> Integer.compare(e1.getOccurrences(), e2.getOccurrences()))
                .orElse(null);
    }

    private NamedEntity getNorthernmostLocation(List<NamedEntity> locations) {
        return locations.stream()
                .filter(nEntity -> nEntity instanceof Location)
                .max((e1, e2) -> Float.compare(((Location) e1).getLatitude(), ((Location) e2).getLatitude()))
                .orElse(null);
    }

    private NamedEntity getSouthernmostLocation(List<NamedEntity> locations) {
        return locations.stream()
                .filter(nEntity -> nEntity instanceof Location)
                .min((e1, e2) -> Float.compare(((Location) e1).getLatitude(), ((Location) e2).getLatitude()))
                .orElse(null);
    }

    private NamedEntity getEasternmostLocation(List<NamedEntity> locations) {
        return locations.stream()
                .filter(nEntity -> nEntity instanceof Location)
                .max((e1, e2) -> Float.compare(((Location) e1).getLongitude(), ((Location) e2).getLongitude()))
                .orElse(null);
    }

    private NamedEntity getWesternmostLocation(List<NamedEntity> locations) {
        return locations.stream()
                .filter(nEntity -> nEntity instanceof Location)
                .min((e1, e2) -> Float.compare(((Location) e1).getLongitude(), ((Location) e2).getLongitude()))
                .orElse(null);
    }

    private NamedEntity getColdestLocation(List<NamedEntity> locations) {
        return locations.stream()
                .filter(nEntity -> nEntity instanceof Location)
                .min((e1, e2) -> Float.compare(((Location) e1).getTemperature(), ((Location) e2).getTemperature()))
                .orElse(null);
    }

    private NamedEntity getHottestLocation(List<NamedEntity> locations) {
        return locations.stream()
                .filter(nEntity -> nEntity instanceof Location)
                .max((e1, e2) -> Float.compare(((Location) e1).getTemperature(), ((Location) e2).getTemperature()))
                .orElse(null);
    }

    /* ************************ PRINTERS ************************ */

    private void printLocationInfo(Location loc) {
        System.out.println("        " + "latitude: " + loc.getLatitude());
        System.out.println("        " + "longitude: " + loc.getLongitude());
        System.out.println("        " + "temperature: " + loc.getTemperature());
    }

    private void printPersonInfo(Person person) {
        System.out.println("        " + "first name: " + person.getName());
        System.out.println("        " + "last name: " + person.getLastName());
    }

    private void printOrganizationInfo(Organization org) {
        System.out.println("        " + "name: " + org.getName());
    }

    private void printExtraStats(Map<String, List<NamedEntity>> personsMap, Map<String, List<NamedEntity>> organizationsMap,
            Map<String, List<NamedEntity>> locationsMap, Map<String, List<NamedEntity>> othersMap) {
        System.out.println("-".repeat(40));
        System.out.println("EXTRA STATS");
        System.out.println("-".repeat(40));

        NamedEntity mostFrequentNamedEntity = getMostFrequentEntity(nEntities);
        System.out.println("    Most Frequent Named Entity: ");
        if (mostFrequentNamedEntity != null) {
            System.out.println("        " + mostFrequentNamedEntity.getLabel() + " (" + mostFrequentNamedEntity.getOccurrences() + ")");
        } else {
            System.out.println("        No entities found.");
        }
        System.out.println("    " + "-".repeat(36));

        NamedEntity mostFrequentPerson = getMostFrequentEntity(
                personsMap.values().stream().flatMap(List::stream).collect(Collectors.toList()));
        System.out.println("    Most Frequent Person: ");
        if (mostFrequentPerson != null) {
            System.out.println("        " + mostFrequentPerson.getLabel() + " (" + mostFrequentPerson.getOccurrences() + ")");
        } else {
            System.out.println("        No persons found.");
        }
        System.out.println("    " + "-".repeat(36));

        NamedEntity mostFrequentOrg = getMostFrequentEntity(
                organizationsMap.values().stream().flatMap(List::stream).collect(Collectors.toList()));
        System.out.println("    Most Frequent Organization: ");
        if (mostFrequentOrg != null) {
            System.out.println("        " + mostFrequentOrg.getLabel() + " (" + mostFrequentOrg.getOccurrences() + ")");
        } else {
            System.out.println("        No organizations found.");
        }
        System.out.println("    " + "-".repeat(36));

        NamedEntity mostFrequentLocation = getMostFrequentEntity(
                locationsMap.values().stream().flatMap(List::stream).collect(Collectors.toList()));
        System.out.println("    Most Frequent Location: ");
        if (mostFrequentLocation != null) {
            System.out.println("        " + mostFrequentLocation.getLabel() + " (" + mostFrequentLocation.getOccurrences() + ")");
        } else {
            System.out.println("        No locations found.");
        }
        System.out.println("    " + "-".repeat(36));

        NamedEntity northernmostLocation = getNorthernmostLocation(
                locationsMap.values().stream().flatMap(List::stream).collect(Collectors.toList()));
        System.out.println("    Northernmost Location: ");
        if (northernmostLocation != null) {
            System.out.println("        " + northernmostLocation.getLabel() + " - Latitude: " + ((Location) northernmostLocation).getLatitude());
        } else {
            System.out.println("        No locations found.");
        }
        System.out.println("    " + "-".repeat(36));

        NamedEntity southernmostLocation = getSouthernmostLocation(
                locationsMap.values().stream().flatMap(List::stream).collect(Collectors.toList()));
        System.out.println("    Southernmost Location: ");
        if (southernmostLocation != null) {
            System.out.println("        " + southernmostLocation.getLabel() + " - Latitude: " + ((Location) southernmostLocation).getLatitude());
        } else {
            System.out.println("        No locations found.");
        }
        System.out.println("    " + "-".repeat(36));

        NamedEntity easternmostLocation = getEasternmostLocation(
                locationsMap.values().stream().flatMap(List::stream).collect(Collectors.toList()));
        System.out.println("    Easternmost Location: ");
        if (easternmostLocation != null) {
            System.out.println("        " + easternmostLocation.getLabel() + " - Longitude: " + ((Location) easternmostLocation).getLongitude());
        } else {
            System.out.println("        No locations found.");
        }
        System.out.println("    " + "-".repeat(36));

        NamedEntity westernmostLocation = getWesternmostLocation(
                locationsMap.values().stream().flatMap(List::stream).collect(Collectors.toList()));
        System.out.println("    Westernmost Location: ");
        if (westernmostLocation != null) {
            System.out.println("        " + westernmostLocation.getLabel() + " - Longitude: " + ((Location) westernmostLocation).getLongitude());
        } else {
            System.out.println("        No locations found.");
        }
        System.out.println("    " + "-".repeat(36));

        NamedEntity coldestLocation = getColdestLocation(
                locationsMap.values().stream().flatMap(List::stream).collect(Collectors.toList()));
        System.out.println("    Coldest Location: ");
        if (coldestLocation != null) {
            System.out.println("        " + coldestLocation.getLabel() + " - Temperature: " + ((Location) coldestLocation).getTemperature());
        } else {
            System.out.println("        No locations found.");
        }
        System.out.println("    " + "-".repeat(36));

        NamedEntity hottestLocation = getHottestLocation(
                locationsMap.values().stream().flatMap(List::stream).collect(Collectors.toList()));
        System.out.println("    Hottest Location: ");
        if (hottestLocation != null) {
            System.out.println("        " + hottestLocation.getLabel() + " - Temperature: " + ((Location) hottestLocation).getTemperature());
        } else {
            System.out.println("        No locations found.");
        }
        System.out.println("-".repeat(40));
    }

    public void printStats(String criterion, String heuristic) {
        Map<String, Map<String, List<NamedEntity>>> nEntitiesMap = getNamedEntitiesMap(criterion);

        nEntitiesMap.forEach((category, entitiesMap) -> {
            System.out.println(category + ":");
            entitiesMap.forEach((key, entities) -> {
                System.out.println("  " + criterion + ": " + key);
                entities.forEach(entity -> {
                    if ((!key.equals("OTHER") || entity.getOccurrences() > 2) || heuristic.equals("Acronym")) {
                        System.out.println("    " + entity.getLabel() + " (" + entity.getOccurrences() + ")");
                        if (entity instanceof Location) printLocationInfo((Location) entity);
                        if (entity instanceof Person) printPersonInfo((Person) entity);
                        if (entity instanceof Organization) printOrganizationInfo((Organization) entity);
                    }
                });
                System.out.println("");
            });
        });

        printExtraStats(nEntitiesMap.get("Persons"), nEntitiesMap.get("Organizations"), nEntitiesMap.get("Locations"), nEntitiesMap.get("Others"));
    }
}