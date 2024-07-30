package feed;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import java.util.ArrayList;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import java.io.StringReader;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import org.w3c.dom.Element;

import org.xml.sax.InputSource;

import utils.FeedsData;

public class FeedParser {

    public static List<Article> parseXML(String xmlData) {
        List<Article> articles = new ArrayList<>();
        Document doc = null;

        try {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            InputSource is = new InputSource(new StringReader(xmlData));
            doc = db.parse(is);
        } catch (Exception e) {
            e.printStackTrace();
        }

        if (doc != null) {
            NodeList itemNodes = doc.getElementsByTagName("item");
            for (int i = 0; i < itemNodes.getLength(); i++) {
                Node itemNode = itemNodes.item(i);
                if (itemNode.getNodeType() == Node.ELEMENT_NODE) {
                    Element itemElement = (Element) itemNode;

                    String title = getElementTextContent(itemElement, "title");
                    String description = getElementTextContent(itemElement, "description");
                    String pubDate = getElementTextContent(itemElement, "pubDate");
                    String link = getElementTextContent(itemElement, "link");

                    Article article = new Article(title, description, pubDate, link);
                    articles.add(article);
                }
            }
        }

        return articles;
    }

    private static String getElementTextContent(Element parent, String tagName) {
        NodeList nodeList = parent.getElementsByTagName(tagName);
        if (nodeList.getLength() > 0) {
            Node node = nodeList.item(0);
            if (node != null) {
                return node.getTextContent();
            }
        }
        return "";
    }

    public static String fetchFeed(String feedURL) throws MalformedURLException, IOException, Exception {

        URL url = new URL(feedURL);
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();

        connection.setRequestMethod("GET");
        connection.setRequestProperty("Content-Type", "application/json");

        connection.setRequestProperty("User-Agent", "Tiesitos Fc");
        connection.setConnectTimeout(5000);
        connection.setReadTimeout(5000);

        int status = connection.getResponseCode();
        if (status != 200) {
            throw new Exception("HTTP error code: " + status);
        } else {
            BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
            String inputLine;
            StringBuffer content = new StringBuffer();
            while ((inputLine = in.readLine()) != null) {
                content.append(inputLine);
            }
            in.close();
            connection.disconnect();
            return content.toString();
        }
    }

    // Returns the parsed articles from a feed
    public static List<Article> processFeed(FeedsData feedData) {
        List<Article> allFeedArticles = new ArrayList<>();
        try {
            String xmlData = FeedParser.fetchFeed(feedData.getUrl());
            List<Article> articles = FeedParser.parseXML(xmlData);
            allFeedArticles.addAll(articles);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return allFeedArticles;
    }
}