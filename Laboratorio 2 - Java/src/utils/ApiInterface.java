package utils;

import java.net.URL;
import java.util.Scanner;
import java.io.IOException;
import java.net.HttpURLConnection;
import org.json.JSONObject;

public class ApiInterface {
    private String apiKey;
    private String location;

    public ApiInterface(String location) {
        this.location = location;
        this.apiKey = "bad7c135e94e44c4933174734242305";
    }

    public JSONObject getInfo() {

        try {
            URL url = new URL("https://api.weatherapi.com/v1/current.json?key=" + apiKey + "&q=" + location);
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("GET");
            connection.connect();

            // Check response status code
            Integer responseCode = connection.getResponseCode();

            if (responseCode != 200) {
                if (responseCode == 404 || responseCode == 400) {
                    System.out.println("Location" + location + "not found");
                    return null;
                }
                throw new RuntimeException("HttpResponseCode: " + responseCode);
            }

            // Scanner to read the data from the API
            StringBuilder inline = new StringBuilder();
            Scanner scanner = new Scanner(url.openStream());

            while (scanner.hasNext()) {
                inline.append(scanner.nextLine());
            }
            scanner.close();

            return new JSONObject(inline.toString());

        } catch (IOException e) {
            e.printStackTrace();
        }

        return null;
    }
}
