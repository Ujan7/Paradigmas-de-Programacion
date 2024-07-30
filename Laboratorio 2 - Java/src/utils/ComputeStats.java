package utils;

import namedEntities.NamedEntity;
import namedEntities.categories.Location;
import namedEntities.categories.Person;
import namedEntities.categories.Organization;

import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;

public class ComputeStats {
    private List<NamedEntity> nEntities;

    public ComputeStats(List<NamedEntity> nEntities) {
        this.nEntities = nEntities;
    }

    /* *********************************************************************** */

    public void addNamedEntityToMap(NamedEntity nEntity, String criterion,
            Map<String, List<NamedEntity>> nEntitiesMap) {
        if (nEntitiesMap.get(criterion) == null) {
            List<NamedEntity> nEntityList = new ArrayList<>();
            nEntityList.add(nEntity);
            nEntitiesMap.put(criterion, nEntityList);
        } else {
            nEntitiesMap.get(criterion).add(nEntity);
        }
    }

    /* ************************ GETTER ************************* */

    Map<String, List<NamedEntity>> getNamedEntitiesMap(String criterion) {
        Map<String, List<NamedEntity>> nEntitiesMap = new HashMap<>();

        for (NamedEntity nEntity : nEntities) {
            if (criterion.equals("Topic")) {
                List<String> topics = nEntity.getTopics();
                for (String topic : topics) {
                    addNamedEntityToMap(nEntity, topic, nEntitiesMap);
                }
            } else {
                String category = EntityProcessor.getNECategory(nEntity);
                addNamedEntityToMap(nEntity, category, nEntitiesMap);
            }
        }
        return nEntitiesMap;
    }

    private NamedEntity getMostFrequentNEntity() {
        NamedEntity mostFrequentNEntity = nEntities.get(0);
        for (NamedEntity entity : nEntities) {
            if (entity.getOccurrences() > mostFrequentNEntity.getOccurrences()) {
                mostFrequentNEntity = entity;
            }
        }
        return mostFrequentNEntity;
    }

    private NamedEntity getMostFrequentPerson() {
        NamedEntity mostFrequentPerson = null;
        for (NamedEntity nEntity : nEntities) {
            if (nEntity instanceof Person) {
                if (mostFrequentPerson == null || nEntity.getOccurrences() > mostFrequentPerson.getOccurrences()) {
                    mostFrequentPerson = nEntity;
                }
            }
        }
        return mostFrequentPerson;
    }

    private NamedEntity getMostFrequentOrganization() {
        NamedEntity mostFrequentOrganization = null;
        for (NamedEntity nEntity : nEntities) {
            if (nEntity instanceof Organization) {
                if (mostFrequentOrganization == null
                        || nEntity.getOccurrences() > mostFrequentOrganization.getOccurrences()) {
                    mostFrequentOrganization = nEntity;
                }
            }
        }
        return mostFrequentOrganization;
    }

    private NamedEntity getMostFrequentLocation() {
        NamedEntity mostFrequentLocation = null;
        for (NamedEntity nEntity : nEntities) {
            if (nEntity instanceof Location) {
                if (mostFrequentLocation == null || nEntity.getOccurrences() > mostFrequentLocation.getOccurrences()) {
                    mostFrequentLocation = nEntity;
                }
            }
        }
        return mostFrequentLocation;
    }

    private NamedEntity getNorthernmostLocation() {
        float maxLatitude = -90.0f;
        NamedEntity northernmostLocation = null;
        for (NamedEntity nEntity : nEntities) {
            if (nEntity instanceof Location) {
                Location entity = (Location) nEntity;
                if (northernmostLocation == null || entity.getLatitude() > maxLatitude) {
                    maxLatitude = entity.getLatitude();
                    northernmostLocation = entity;
                }
            }
        }
        return northernmostLocation;
    }

    private NamedEntity getSouthernmostLocation() {
        float minLatitude = 90.0f;
        NamedEntity southernmostLocation = null;
        for (NamedEntity nEntity : nEntities) {
            if (nEntity instanceof Location) {
                Location entity = (Location) nEntity;
                if (southernmostLocation == null || entity.getLatitude() < minLatitude) {
                    minLatitude = entity.getLatitude();
                    southernmostLocation = entity;
                }
            }
        }
        return southernmostLocation;
    }

    private NamedEntity getEasternmostLocation() {
        float maxLongitude = -180.0f;
        NamedEntity easternmostLocation = null;
        for (NamedEntity nEntity : nEntities) {
            if (nEntity instanceof Location) {
                Location entity = (Location) nEntity;
                if (easternmostLocation == null || entity.getLongitude() > maxLongitude) {
                    maxLongitude = entity.getLongitude();
                    easternmostLocation = entity;
                }
            }
        }
        return easternmostLocation;
    }

    private NamedEntity getWesternmostLocation() {
        float minLongitude = 180.0f;
        NamedEntity westernmostLocation = null;
        for (NamedEntity nEntity : nEntities) {
            if (nEntity instanceof Location) {
                Location entity = (Location) nEntity;
                if (westernmostLocation == null || entity.getLongitude() < minLongitude) {
                    minLongitude = entity.getLongitude();
                    westernmostLocation = entity;
                }
            }
        }
        return westernmostLocation;
    }

    private NamedEntity getColdestLocation() {
        float minTemperature = Float.MAX_VALUE;
        NamedEntity coldestLocation = null;
        for (NamedEntity nEntity : nEntities) {
            if (nEntity instanceof Location) {
                Location entity = (Location) nEntity;
                if (coldestLocation == null || entity.getTemperature() < minTemperature) {
                    minTemperature = entity.getTemperature();
                    coldestLocation = entity;
                }
            }
        }
        return coldestLocation;
    }

    private NamedEntity getHottestLocation() {
        float maxTemperature = Float.MIN_VALUE;
        NamedEntity hottestLocation = null;
        for (NamedEntity nEntity : nEntities) {
            if (nEntity instanceof Location) {
                Location entity = (Location) nEntity;
                if (hottestLocation == null || entity.getTemperature() > maxTemperature) {
                    maxTemperature = entity.getTemperature();
                    hottestLocation = entity;
                }
            }
        }
        return hottestLocation;
    }

    /* ************************ PRINTERS ************************ */

    private void printLocationInfo(NamedEntity nEntity) {
        if (nEntity instanceof Location) {
            Location loc = (Location) nEntity;
            System.out.println("        " + "latitude: " + loc.getLatitude());
            System.out.println("        " + "longitude: " + loc.getLongitude());
            System.out.println("        " + "temperature: " + loc.getTemperature());
        }
    }

    private void printPersonInfo(NamedEntity nEntity) {
        if (nEntity instanceof Person) {
            Person person = (Person) nEntity;
            System.out.println("        " + "first name: " + person.getName());
            System.out.println("        " + "last name: " + person.getLastName());
        }
    }

    private void printOrganizationInfo(NamedEntity nEntity) {
        if (nEntity instanceof Organization) {
            Organization org = (Organization) nEntity;
            System.out.println("        " + "name: " + org.getName());
        }
    }

    private void printExtraStats(Map<String, List<NamedEntity>> nEntitiesMap) {
        System.out.println("-".repeat(40));
        System.out.println("EXTRA STATS");
        System.out.println("-".repeat(40));

        NamedEntity mostFrequentNamedEntity = getMostFrequentNEntity();
        System.out.println("    Most Frequent Named Entity: ");
        System.out.println("        " + mostFrequentNamedEntity.getLabel() + " ("
                + mostFrequentNamedEntity.getOccurrences() + ")");
        System.out.println("    " + "-".repeat(36));

        NamedEntity mostFrequentPerson = getMostFrequentPerson();
        System.out.println("    Most Frequent Person: ");
        if (mostFrequentPerson != null) {
            System.out.println(
                    "        " + mostFrequentPerson.getLabel() + " (" + mostFrequentPerson.getOccurrences() + ")");
        } else {
            System.out.println("        No persons found.");
        }
        System.out.println("    " + "-".repeat(36));

        NamedEntity mostFrequentOrg = getMostFrequentOrganization();
        System.out.println("    Most Frequent Organization: ");
        if (mostFrequentOrg != null) {
            System.out.println("        " + mostFrequentOrg.getLabel() + " (" + mostFrequentOrg.getOccurrences() + ")");
        } else {
            System.out.println("        No organizations found.");
        }
        System.out.println("    " + "-".repeat(36));

        NamedEntity mostFrequentLocation = getMostFrequentLocation();
        System.out.println("    Most Frequent Location: ");
        if (mostFrequentLocation != null) {
            System.out.println(
                    "        " + mostFrequentLocation.getLabel() + " (" + mostFrequentLocation.getOccurrences() + ")");
        } else {
            System.out.println("        No locations found.");
        }
        System.out.println("    " + "-".repeat(36));

        NamedEntity northernmostLocation = getNorthernmostLocation();
        System.out.println("    Northernmost Location: ");
        if (northernmostLocation != null) {
            System.out.println("        " + northernmostLocation.getLabel() + " - Latitude: "
                    + ((Location) northernmostLocation).getLatitude());
        } else {
            System.out.println("        No locations found.");
        }
        System.out.println("    " + "-".repeat(36));

        NamedEntity southernmostLocation = getSouthernmostLocation();
        System.out.println("    Southernmost Location: ");
        if (southernmostLocation != null) {
            System.out.println("        " + southernmostLocation.getLabel() + " - Latitude: "
                    + ((Location) southernmostLocation).getLatitude());
        } else {
            System.out.println("        No locations found.");
        }
        System.out.println("    " + "-".repeat(36));

        NamedEntity easternmostLocation = getEasternmostLocation();
        System.out.println("    Easternmost Location: ");
        if (easternmostLocation != null) {
            System.out.println("        " + easternmostLocation.getLabel() + " - Longitude: "
                    + ((Location) easternmostLocation).getLongitude());
        } else {
            System.out.println("        No locations found.");
        }
        System.out.println("    " + "-".repeat(36));

        NamedEntity westernmostLocation = getWesternmostLocation();
        System.out.println("    Westernmost Location: ");
        if (westernmostLocation != null) {
            System.out.println("        " + westernmostLocation.getLabel() + " - Longitude: "
                    + ((Location) westernmostLocation).getLongitude());
        } else {
            System.out.println("        No locations found.");
        }
        System.out.println("    " + "-".repeat(36));

        NamedEntity coldestLocation = getColdestLocation();
        System.out.println("    Coldest Location: ");
        if (coldestLocation != null) {
            System.out.println("        " + coldestLocation.getLabel() + " - Temperature: "
                    + ((Location) coldestLocation).getTemperature());
        } else {
            System.out.println("        No locations found.");
        }
        System.out.println("    " + "-".repeat(36));

        NamedEntity hottestLocation = getHottestLocation();
        System.out.println("    Hottest Location: ");
        if (hottestLocation != null) {
            System.out.println("        " + hottestLocation.getLabel() + " - Temperature: "
                    + ((Location) hottestLocation).getTemperature());
        } else {
            System.out.println("        No locations found.");
        }
        System.out.println("-".repeat(40));

    }

    public void printStats(String criterion, String heuristic) {
        Map<String, List<NamedEntity>> nEntitiesMap = getNamedEntitiesMap(criterion);

        for (Map.Entry<String, List<NamedEntity>> entry : nEntitiesMap.entrySet()) {
            System.out.println(criterion + ": " + entry.getKey());

            for (NamedEntity entity : entry.getValue()) {
                if ((!entry.getKey().equals("OTHER") || entity.getOccurrences() > 2) || heuristic.equals("Acronym")) {
                    System.out.println("    " + entity.getLabel() + " (" + entity.getOccurrences() + ")");
                    printLocationInfo(entity); // Extra stats for LOCATION category
                    printPersonInfo(entity); // Extra stats for PERSON category
                    printOrganizationInfo(entity); // Extra stats for ORGANIZATION category
                }
            }

            System.out.println("");
        }
        printExtraStats(nEntitiesMap);
    }
}
