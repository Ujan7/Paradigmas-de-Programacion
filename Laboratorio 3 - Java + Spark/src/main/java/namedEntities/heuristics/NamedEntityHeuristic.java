package namedEntities.heuristics;
import java.io.Serializable;
import java.util.List;

public abstract class NamedEntityHeuristic implements Serializable {
    public abstract List<String> extractCandidates(String text);

    public static NamedEntityHeuristic selectHeuristic(String heuristicName) {
        switch (heuristicName) {
            case "CapitalizedWord":
                return new CapitalizedWordHeuristic();
            case "MixedCaseWord":
                return new MixedCaseWordHeuristic();
            case "Acronym":
                return new AcronymHeuristic();
            default:
                throw new IllegalArgumentException("Heuristica desconocida: " + heuristicName);
        }
    }
}