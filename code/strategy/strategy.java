interface ArchiveStrategy {
    // For simplicity, represent file and compressed file as string
    public String archive(String[] files);

    public String getArchiveExtension();
}

class ZipArchiveStrategy implements ArchiveStrategy {
    public String archive(String[] files) {
        return "archived to compressed zip";
    }

    public String getArchiveExtension() {
        return "zip";
    }
}

class TarArchiveStrategy implements ArchiveStrategy {
    public String archive(String[] files) {
        return "archived to tar";
    }

    public String getArchiveExtension() {
        return "tar";
    }
}

class ArchiveContext {
    private ArchiveStrategy strategy;

    public void setStrategy(ArchiveStrategy strategy) {
        this.strategy = strategy;
    }

    public String archive(String[] files, String archiveName) {
        if(this.strategy != null) {
            return this.strategy.archive(files) + ", " + archiveName + "." + this.strategy.getArchiveExtension();
        } else {
            return "";
        }
    }
}

class Main {
    public static void main(String[] args) {
        // code...
        // Set strategy at run time
        ArchiveContext ctx = new ArchiveContext();
        ctx.setStrategy(new ZipArchiveStrategy());
        String archive = ctx.archive(new String[] {"File1", "File2"}, "zipfile1");
        System.out.println(archive);

        // Change strategy
        ctx.setStrategy(new TarArchiveStrategy());
        archive = ctx.archive(new String[] {"File1", "File2"}, "tarfile1");
        System.out.println(archive);
    }
}