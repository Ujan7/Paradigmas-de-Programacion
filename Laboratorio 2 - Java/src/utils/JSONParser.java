package utils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import namedEntities.categories.Location;

import org.json.JSONArray;
import org.json.JSONObject;

public class JSONParser {

    static public List<FeedsData> parseJsonFeedsData(String jsonFilePath) throws IOException {
        String jsonData = new String(Files.readAllBytes(Paths.get(jsonFilePath)));
        List<FeedsData> feedsList = new ArrayList<>();

        JSONArray jsonArray = new JSONArray(jsonData);
        for (int i = 0; i < jsonArray.length(); i++) {
            JSONObject jsonObject = jsonArray.getJSONObject(i);
            String label = jsonObject.getString("label");
            String url = jsonObject.getString("url");
            String type = jsonObject.getString("type");
            feedsList.add(new FeedsData(label, url, type));
        }
        return feedsList;
    }

    // Returns all the entities from the json in a Entity list
    static public List<Entity> parseJsonEntitiesData(String jsonFilePath) throws IOException {
        String jsonData = new String(Files.readAllBytes(Paths.get(jsonFilePath)));
        List<Entity> entitiesList = new ArrayList<>();

        JSONArray jsonArray = new JSONArray(jsonData);
        for (int i = 0; i < jsonArray.length(); i++) {
            JSONObject jsonObject = jsonArray.getJSONObject(i);
            String label = jsonObject.getString("label");
            String category = jsonObject.getString("Category");
            JSONArray topicsArray = jsonObject.getJSONArray("Topics");
            JSONArray keywordsArray = jsonObject.getJSONArray("keywords");

            List<String> topics = new ArrayList<>();
            for (int j = 0; j < topicsArray.length(); j++) {
                topics.add(topicsArray.getString(j));
            }

            List<String> keywords = new ArrayList<>();
            for (int k = 0; k < keywordsArray.length(); k++) {
                keywords.add(keywordsArray.getString(k));
            }

            entitiesList.add(new Entity(label, category, topics, keywords));
        }
        return entitiesList;
    }

    public static Location parseLocInfo(JSONObject info, String label, List<String> topics) {
        JSONObject location = info.getJSONObject("location");
        JSONObject current = info.getJSONObject("current");
        Float latitude = location.getFloat("lat");
        Float longitude = location.getFloat("lon");
        Float temperature = current.getFloat("temp_c");

        return new Location(label, topics, 1, latitude, longitude, temperature);
    }
}
