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
import optimiser.abstracter
import optimiser.std
import java.nio.file.Paths

object Main extends App {
  def getFilesWithExtension(dir: File, extension: String): Array[File] = {
    val these = dir.listFiles
    val good =
      these.filter(file => file.isFile && file.getName.endsWith(extension))
    good ++ these
      .filter(_.isDirectory)
      .flatMap(dir => getFilesWithExtension(dir, extension))
  }

  def help() = {
    println("""usage: mink [options] <input> [options]
       mink -v
options:
  -h          print this help message
  -V          verbose mode
  -o <file>   output file
  -N          disable clang optimisation
  -O <level>  optimisation level (default 4)""")
    sys.exit(1)
  }

  // Parse arguments

  var inputFilename: Option[String] = None
  var outputFilename: Option[String] = None
  var optimisationLevel: Int = 4
  var clangOptimisation: Boolean = true
  var parsedArgs = args.toList
  var verbose = false
  while (parsedArgs.nonEmpty) {
    parsedArgs match {
      case "-V" :: rest => {
        verbose = true
        parsedArgs = rest
      }
      case "-v" :: rest => {
        println("mink 1.0.0")
        sys.exit(0)
      }
      case "-o" :: rest => {
        rest match {
          case Nil => {
            println("missing argument(s) for flag -o")
            help()
          }
          case o :: rest => {
            outputFilename = Some(o)
            parsedArgs = rest
          }
        }
      }
      case "-h" :: rest => {
        help()
      }
      case "-N" :: rest => {
        clangOptimisation = false
        parsedArgs = rest
      }
      case "-O" :: l :: rest => {
        optimisationLevel = l.toIntOption.getOrElse(0)
        parsedArgs = rest
      }
      case i :: rest => {
        if (i.startsWith("-")) {
          println(s"unknown flag $i")
          help()
        }
        inputFilename = Some(i)
        parsedArgs = rest
      }
      case Nil => {}
    }
  }

  if (inputFilename.isEmpty) {
    help()
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
  val parsedMain = parser.parse(
    file.getName(),
    Paths.get(workingDir).relativize(Paths.get(file.toURI()).getParent()),
    fileContent
  ) match {
    case parsley.Failure(error) =>
      println(error)
      parsingFailed = true
      None
    case parsley.Success(parsedProgram) =>
      Some(parsedProgram)
  }

  for (f <- allFiles) {
    val fileContent = Source.fromFile(f).getLines().mkString("\n")
    parser.parse(
      f.getName(),
      Paths.get(workingDir).relativize(Paths.get(f.toURI).getParent()),
      fileContent
    ) match {
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

  val parsedFiles =
    (std.standardLibrary.map(x => (x.packageName.get, x)) ++ pf.toList)
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
      val (checkedProgram, _, _) = checker.checkProgram(parsedProgram)
      checkedProgram
    })
    .toList

  val parsedProgram = parsedMain.get
  val (checkedProgram, checkedErrors, cImports) =
    checker.checkProgram(parsedProgram)
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

  val chainedProgram = chain.mergeOnes(
    inlinedProgram
  )

  val (tailCallOptimisedProgram, tailCallErrors) =
    if (optimisationLevel <= 0) (chainedProgram, Nil)
    else {
      (new tailcall()).tailCall(inlinedProgram)
    }

  tailCallErrors match {
    case Nil =>
    case errors => {
      errors.foreach(println)
      sys.exit(1)
    }
  }

  val abstractedProgram =
    abstracter.scanProgram(tailCallOptimisedProgram, optimisationLevel)

  if (verbose) println(abstractedProgram)

  // Generate C code and store in a temporary file

  val generatedCode =
    (new codegen()).generateProgram(abstractedProgram, cImports)

  val inputFileName = file.getName()

  val tempFile = File.createTempFile(
    inputFileName.substring(0, inputFileName.lastIndexOf(".")),
    ".c",
    workingDirFile.get
  )
  val writer = new BufferedWriter(new FileWriter(tempFile))
  writer.write(generatedCode)
  writer.close()

  val inputFilePath = file.getPath()
  val outputFile = outputFilename.getOrElse(
    inputFilePath.substring(0, inputFilePath.lastIndexOf("."))
  )

  val clangCommand =
    if (!clangOptimisation)
      s"clang ${tempFile.getAbsolutePath} -O0 -o ${outputFile}"
    else
      s"clang ${tempFile.getAbsolutePath} -Ofast -o ${outputFile}"
  val exitCode = clangCommand.!

  if (exitCode != 0) {
    println("compilation error:\n  clang failed to compile")
    sys.exit(1)
  }

  tempFile.delete()
}
