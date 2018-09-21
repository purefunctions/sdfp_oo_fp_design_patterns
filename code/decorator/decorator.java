interface Image {
    // Returning string for the sake of a demo
    public String getImage();
}

class PatternedImage implements Image {
    private final String description;

    public PatternedImage(String desc) {
        description = desc;
    }

    @Override
    public String getImage() {
        return description;
    }
}

abstract class ImageDecorator implements Image {
    protected Image toBeDecorated;

    public ImageDecorator(Image toBeDecorated) {
        this.toBeDecorated = toBeDecorated;
    }

    @Override
    public String getImage() {
        return toBeDecorated.getImage();
    }
}

class ContrastFilter extends ImageDecorator {
    protected double factor;

    public ContrastFilter(double factor, Image toBeDecorated) {
        super(toBeDecorated);
        this.factor = factor;
    }

    @Override
    public String getImage() {
        return super.getImage() + " + Contrast factor " + factor;
    }
}

class MonochromeFilter extends ImageDecorator {
    public MonochromeFilter(Image toBeDecorated) {
        super(toBeDecorated);
    }

    @Override
    public String getImage() {
        return super.getImage() + " + Monochrome conversion";
    }
}

class EdgeDetectionFilter extends ImageDecorator {
    public EdgeDetectionFilter(Image toBeDecorated) {
        super(toBeDecorated);
    }

    @Override
    public String getImage() {
        return super.getImage() + " + Edge detection";
    }
}

class Main {
    public static void main(String[] args) {
        Image img = new PatternedImage("Mosaic Image");

        Image transformedImage = new EdgeDetectionFilter(
            // new MonochromeFilter(
                new ContrastFilter(
                    2.5,
                    img
                )
            // )
        );

        System.out.println(transformedImage.getImage());
    }
}