import parser.program
import java.io.File
import scala.util.Success
import scala.util.Failure
import semantic.checker
import optimiser.recursion
import optimiser.inliner
import scala.io.Source
import ast.Ident
import ast.Program
import collection.mutable
import optimiser.tailcall
import optimiser.chain
import codegen.codegen
import java.io.BufferedWriter
import java.io.FileWriter
import scala.sys.process._

object Main extends App {
  def getFilesWithExtension(dir: File, extension: String): Array[File] = {
    val these = dir.listFiles
    val good =
      these.filter(file => file.isFile && file.getName.endsWith(extension))
    good ++ these
      .filter(_.isDirectory)
      .flatMap(dir => getFilesWithExtension(dir, extension))
  }

  // Parse arguments

  var inputFilename: Option[String] = None
  var outputFilename: Option[String] = None
  var parsedArgs = args.toList
  while (parsedArgs.nonEmpty) {
    parsedArgs match {
      case "-o" :: o :: rest => {
        outputFilename = Some(o)
        parsedArgs = rest
      }
      case i :: rest => {
        inputFilename = Some(i)
        parsedArgs = rest
      }
      case Nil => {}
    }
  }

  if (inputFilename.isEmpty) {
    println("Usage: mink <input>")
    sys.exit(1)
  }

  val input = inputFilename.get
  val file = new File(input)

  if (!file.exists()) {
    println(s"input error\n  $input: file does not exist")
    sys.exit(1)
  }

  // Get all known mink files

  val workingDir = System.getProperty("user.dir")
  var workingDirFile: Option[File] = Some(new File(workingDir))

  if (!workingDirFile.get.exists() || !workingDirFile.get.isDirectory()) {
    workingDirFile = None
  }

  val allFiles =
    workingDirFile.map(getFilesWithExtension(_, ".mink")).getOrElse(Array())

  // Parse all files and store by package name

  val pf = mutable.ListBuffer[(Ident, Program)]()
  var parsingFailed = false
  var parser = new program()

  val fileContent = Source.fromFile(file).getLines().mkString("\n")
  val parsedMain = parser.parse(file.getName(), fileContent) match {
    case parsley.Failure(error) =>
      println(error)
      parsingFailed = true
      None
    case parsley.Success(parsedProgram) =>
      Some(parsedProgram)
  }

  for (f <- allFiles) {
    val fileContent = Source.fromFile(f).getLines().mkString("\n")
    parser.parse(f.getName(), fileContent) match {
      case parsley.Failure(error) =>
        println(error)
        parsingFailed = true
      case parsley.Success(parsedProgram) =>
        parsedProgram.packageName match {
          case Some(packageName) =>
            pf += ((packageName, parsedProgram))
          case None => {}
        }
    }
  }

  if (parsingFailed) {
    sys.exit(1)
  }

  // Merge all files with same package name

  val parsedFiles = pf.toList
    .groupBy(_._1)
    .map({ case (k, v) =>
      (
        k,
        Program(
          Some(k),
          v.flatMap(_._2.imports).distinct,
          v.flatMap(_._2.instructions),
          v.flatMap(_._2.funcs)
        )
      )
    })

  // Semantically check all packages

  val checker = new checker(parsedFiles)
  var checkingFailed = false
  val checkedPrograms = parsedFiles
    .map({ case (_, parsedProgram) =>
      val (checkedProgram, checkedErrors) = checker.checkProgram(parsedProgram)
      checkedErrors match {
        case Nil =>
        case errors => {
          errors.foreach(println)
          checkingFailed = true
        }
      }
      checkedProgram
    })
    .toList

  val parsedProgram = parsedMain.get
  val (checkedProgram, checkedErrors) =
    (new checker(parsedFiles)).checkProgram(parsedProgram)
  checkedErrors match {
    case Nil =>
    case errors => {
      errors.foreach(println)
      checkingFailed = true
    }
  }

  if (checkingFailed) {
    sys.exit(1)
  }

  // Merge packages and main file into one program

  val mergedProgram = Program(
    None,
    Nil,
    checkedProgram.instructions,
    (checkedProgram :: checkedPrograms)
      .flatMap(p => {
        p.funcs
      })
  )

  // Perform a series of optimisations

  val inlinedProgram = (new inliner()).inlineFunctions(mergedProgram)

  val (tailCallOptimisedProgram, tailCallErrors) =
    (new tailcall()).tailCall(inlinedProgram)
  tailCallErrors match {
    case Nil =>
    case errors => {
      errors.foreach(println)
      sys.exit(1)
    }
  }

  val chainedProgram = chain.mergeOnes(tailCallOptimisedProgram)

  // Generate C code and store in a temporary file

  val generatedCode = (new codegen()).generateProgram(chainedProgram)

  val inputFileName = file.getName()

  val tempFile = File.createTempFile(
    inputFileName.substring(0, inputFileName.lastIndexOf(".")),
    ".c"
  )
  val writer = new BufferedWriter(new FileWriter(tempFile))
  writer.write(generatedCode)
  writer.close()

  val inputFilePath = file.getPath()
  val outputFile = outputFilename.getOrElse(
    inputFilePath.substring(0, inputFilePath.lastIndexOf("."))
  )

  val clangCommand = s"clang ${tempFile.getAbsolutePath} -o ${outputFile}"
  val exitCode = clangCommand.!

  if (exitCode != 0) {
    println("compilation error:\n  clang failed to compile")
    sys.exit(1)
  }

  tempFile.delete()
}
