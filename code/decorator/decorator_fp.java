import java.util.function.*;
import java.util.stream.*;

@SuppressWarnings("unchecked")
class MainFP {
    public static String contrastFilter(double factor, String img) {
        return img + " + Contrast factor " + factor;
    }

    public static String monochromeFilter(String img) {
        return img + " + Monochrome conversion";
    }

    public static String edgeDetectionFilter(String img) {
        return img + " + Edge detection";
    }

    public static Function<String, String> combineFilters(Function<String, String>... filters) {
        return Stream.of(filters).reduce(Function.identity(), Function::andThen);
    }

    public static void main(String[] args) {
        Function<String, String> contrastEnhancer = (String img) -> {return contrastFilter(2.5, img);};
        // Function<String, String> filterChain = contrastEnhancer
        //     .andThen(MainFP::monochromeFilter)
        //     .andThen(MainFP::edgeDetectionFilter);

        Function<String, String> filterChain = MainFP.combineFilters(contrastEnhancer, MainFP::monochromeFilter, MainFP::edgeDetectionFilter);

        String transformedImage = filterChain.apply("Mosaic image");
        System.out.println(transformedImage);
    }
}