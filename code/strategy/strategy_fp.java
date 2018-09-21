import java.util.function.*;

class MainFP {
    // Equivalent of a combination of ArchiveStrategy (function signature) 
    // and ArchiveContext (usage of function parameters) along with its setContext
    public static String archive(String[] files, String archiveName, String extension, Function<String[], String> compressFunction) {
        return compressFunction.apply(files) + ", " + archiveName + "." + extension;
    }

    // Elsewhere in the code, even in a separate library, the implementation of strategy
    public static String archiveToTar(String[] files, String archiveName) {
        return MainFP.archive(files, archiveName, "tar", files_ -> "archived to tar");
    }

    public static String archiveToZip(String[] files, String archiveName) {
        return MainFP.archive(files, archiveName, "zip", files_ -> "compressed to zip");
    }

    public static void main(String[] args) {
        System.out.println(
            MainFP.archiveToTar(new String [] {"file1", "file2"}, "tarfile1")
        );

        System.out.println(
            MainFP.archiveToZip(new String [] {"file1", "file2"}, "zipfile1")
        );
    }
}