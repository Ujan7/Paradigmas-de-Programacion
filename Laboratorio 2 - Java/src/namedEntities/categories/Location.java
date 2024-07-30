package namedEntities.categories;

import java.util.List;

import namedEntities.NamedEntity;

public class Location extends NamedEntity {
    private float latitude;
    private float longitude;
    private float temperature;

    // Constructor
    public Location(String label, List<String> topics, int occurrences) {
        super(label, topics, occurrences);
    }

    public Location(String label, List<String> topics, int occurrences, float latitude, float longitude,
            Float temperature) {
        super(label, topics, occurrences);
        this.latitude = latitude;
        this.longitude = longitude;
        this.temperature = temperature;
    }

    // Getters
    public float getLatitude() {
        return latitude;
    }

    public float getLongitude() {
        return longitude;
    }

    public float getTemperature() {
        return temperature;
    }

    // Setters
    public void setLatitude(float latitude) {
        this.latitude = latitude;
    }

    public void setLongitude(float longitude) {
        this.longitude = longitude;
    }

    public void setTemperature(float temperature) {
        this.temperature = temperature;
    }
}
