package utils;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import feed.Article;

public class FileHelper {

    public static void articlesToFile(List<Article> articles, String directoryPath, String fileName) {
        try {
            Path filePath = Paths.get(directoryPath, fileName);
            FileWriter fileWriter = new FileWriter(filePath.toString());

            for (Article article : articles) {
                String title = article.getTitle();
                String description = article.getDescription();
                // Not including "Title" and "Description" in the file 
                fileWriter.write(title + "\n");
                fileWriter.write(description + "\n");
            }

            fileWriter.close();
            System.out.println("File created successfully in path" + filePath);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
