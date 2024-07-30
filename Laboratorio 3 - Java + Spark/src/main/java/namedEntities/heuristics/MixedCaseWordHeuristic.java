package namedEntities.heuristics;

import java.util.List;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.text.Normalizer;

public class MixedCaseWordHeuristic extends NamedEntityHeuristic {
    @Override
    public List<String> extractCandidates(String text) {
        List<String> candidates = new ArrayList<>();

        text = text.replaceAll("[-+.^:,\"]", "");
        text = Normalizer.normalize(text, Normalizer.Form.NFD);
        text = text.replaceAll("\\p{M}", "");
        text = text.replaceAll(
                "(?i)\\b(el|la|con|en|de|del|los|las|un|una|al|por|para|son|si|no|se|porque|a|este|esta|estos|como|lo|su|sus|tu|tus|sobre|entre|tras|antes|después|hasta|durante|según|mediante)\\b",
                "");

        Pattern pattern = Pattern.compile("[A-Z]+[a-z]*[A-Z]*");
        Matcher matcher = pattern.matcher(text);

        while (matcher.find()) {
            candidates.add(matcher.group());
        }

        return candidates;
    }
}
