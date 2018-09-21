interface MediaElement {
    void accept(MediaElementVisitor visitor);
}

interface MediaElementVisitor {
    // Note: There's no way to add more data types without changing code
    void visit(ImageType svg);
    void visit(AudioType audio);
    void visit(VideoType video);
    void visit(MediaList list);
}

class MediaList implements MediaElement {
    private MediaElement[] mediaElements;
    private String listName;

    MediaList(MediaElement[] mediaElements, String listName) {
        this.mediaElements = mediaElements;
        this.listName = listName;
    }

    @Override
    public void accept(MediaElementVisitor visitor) {
        for (MediaElement element : mediaElements) {
            element.accept(visitor);
        }
        visitor.visit(this);
    }

    public String getName() {
        return listName;
    }
}

class ImageType implements MediaElement {
    // ImageType specific code
    // ...

    // Implement accept method to be visitable by any class implementing MediaElementVisitor
    @Override
    public void accept(MediaElementVisitor visitor) {
        visitor.visit(this);
    }
}

class AudioType implements MediaElement {
    // AudioType specific code
    // ...

    // Implement accept method to be visitable by any class implementing MediaElementVisitor
    @Override
    public void accept(MediaElementVisitor visitor) {
        visitor.visit(this);
    }
}

class VideoType implements MediaElement {
    // VideoType specific code
    // ...

    // Implement accept method to be visitable by any class implementing MediaElementVisitor
    @Override
    public void accept(MediaElementVisitor visitor) {
        visitor.visit(this);
    }
}

// New operations on the classes that can be implemented elsewhere without touching the classes themselves

class MediaElementDisplayVisitor implements MediaElementVisitor {
    @Override
    public void visit(MediaList list) {
        System.out.println("Displaying media list: " + list.getName());
    }

    @Override
    public void visit(ImageType image) {
        System.out.println("Displaying image");
    }

    @Override
    public void visit(AudioType audio) {
        System.out.println("Displaying audio");
    }

    @Override
    public void visit(VideoType video) {
        System.out.println("Displaying video");
    }
}

class MediaElementLikeVisitor implements MediaElementVisitor {
    @Override
    public void visit(MediaList list) {
        System.out.println("Liking media list: " + list.getName());
    }

    @Override
    public void visit(ImageType image) {
        System.out.println("Liking image");
    }

    @Override
    public void visit(AudioType audio) {
        System.out.println("Liking audio");
    }

    @Override
    public void visit(VideoType video) {
        System.out.println("Liking video");
    }
}

class Main {
    public static void main(String[] args) {
        MediaList mediaList = new MediaList(
            new MediaElement[] {new ImageType(), new AudioType(), new ImageType(), new VideoType()},
            "An awesome list of curated media"
        );
        
        mediaList.accept(new MediaElementDisplayVisitor());
        System.out.println("\n");
        mediaList.accept(new MediaElementLikeVisitor());
    }
}