package templ

import java.io.{FilenameFilter, File}
import java.lang.String
import templ.Value._

object Module {
  def loadModules(directory: File): Value = {
    val sources = directory.listFiles(new FilenameFilter() {
      def accept(dir: File, name: String) =
        Character.isUpperCase(name.charAt(0)) &&
        name.endsWith(".t") && new File(dir, name).isFile
    })
    val fields = for(source <- List.fromArray(sources)) yield {
      val e = Parser.parseFile(source.getAbsolutePath)
      (source.getName.dropRight(2) -> VLambda(Map(), "data", e))
    }
    val directories = directory.listFiles(new FilenameFilter() {
      def accept(dir: File, name: String) =
        Character.isUpperCase(name.charAt(0)) &&
        new File(dir, name).isDirectory
    })
    val modules = for(directory <- List.fromArray(directories)) yield {
      if(fields.contains(directory.getName)) {
        throw new RuntimeException("Module " + directory + " is both a directory and a source file");
      }
      (directory.getName -> loadModules(directory))
    }
    VRecord(Map((fields ++ modules): _*))
  }
}
