package isabelle


import isabelle.*
import isabelle.utils.Lines


object Theory_Chunks {

  def chunk_spans(spans0: List[String], size: Int): List[String] = {
    val spans = spans0.dropWhile(_.isBlank)
    def chunk_one(spans: List[String], remaining: Int): (String, List[String]) = spans match {
      case head :: spans =>
        if (head.length == remaining) (head, spans)
        else if (head.length > remaining)
          (split_lines(head.take(remaining + 1)).dropRight(1).mkString("\n"), spans)
        else {
          val (chunk, rest) = chunk_one(spans, remaining - head.length)
          (head + chunk, rest)
        }
      case Nil => ("", Nil)
    }

    if (spans.isEmpty) Nil
    else {
      val (chunk, res) = chunk_one(spans, size)
      chunk :: chunk_spans(res, size)
    }
  }

  def prune_ws(spans: List[String]): List[String] =
    spans match {
      case head :: rest if head.isBlank => head :: prune_ws(rest.dropWhile(_.isBlank))
      case head :: rest => head :: prune_ws(rest)
      case Nil => Nil
    }

  def theory_chunks(
    options: Options,
    input_dir: Path,
    output_dir: Path,
    dirs: List[Path],
    select_dirs: List[Path],
    selection: Sessions.Selection,
    progress: Progress = new Progress
  ): Unit = {
    Isabelle_System.make_directory(output_dir)

    val store = Sessions.store(options)
    val full_sessions = Sessions.load_structure(options, dirs, select_dirs)
    val deps = full_sessions.selection_deps(selection, progress, verbose = true)
    val sessions = deps.sessions_structure.build_selection(selection)

    val example_thys = split_lines(File.read(input_dir + Path.basic("examples.csv"))).drop(1).toSet
    val line_counts = split_lines(File.read(input_dir + Path.basic("line-count.csv"))).drop(1)
    val tools = split_lines(File.read(input_dir + Path.basic("tools.csv"))).drop(1).toSet
    val thys = line_counts.map(_.split(",")).filterNot(_(2) == "0").map(_(1)).filterNot(example_thys.contains).toSet

    val before = "When assessing the quality of an Isabelle formalization, we can always be totally assured that it is correct.\n" +
      "However, quality can vary: Formalizations need to be understandable, maintainable, and easy to use.\n" +
      "Unclear definitions, proofs that are too hard to follow, and proofs that are likely to break with future changes are all problematic and should be avoided.\n" +
      "Let us take the following snippet as an example:\n\n"
    val after = "\nend\n\nOf course some definitions and lemmas are missing as this is just a snippet. It works and is correct. As for the quality, on a scale of 1 to 10, I would give it a "
    val chunk_size = 2048 - (before.length + after.length)

    val res = sessions.filterNot(tools.contains).flatMap { session_name =>
      using(Export.open_session_context0(store, session_name)) { session_context =>
        val proper_session_theories =
          deps(session_name).proper_session_theories.map(_.theory).filter(thys.contains)

        proper_session_theories.flatMap { theory_name =>
          progress.echo("Processing " + theory_name)
          val context = session_context.theory(theory_name)
          val markup = context.uncompressed_yxml(Export.MARKUP)
          val spans = prune_ws(markup.map(Lines.source))
          chunk_spans(spans, chunk_size).drop(1).filterNot(_.isBlank).map(chunk =>
            theory_name -> (before + chunk.trim + after))
        }
      }
    }

    val (thy_names, chunks) = res.unzip
    progress.echo("Created " + chunks.length + " chunks")
    File.write_gzip(output_dir + Path.basic("thy_names.gz"), thy_names.mkString("\n"))
    File.write_gzip(output_dir + Path.basic("chunks.gz"), chunks.map(_.replace("\n", "\\n")).mkString("\n"))
  }


  /* isabelle tools */

  val isabelle_tool = Isabelle_Tool("theory_chunks", "chunk theory into prompts",
    Scala_Project.here,
    { args =>
      var base_sessions: List[String] = Nil
      var select_dirs: List[Path] = Nil
      var input_dir = Path.explode("data")
      var requirements = false
      var output_dir = Path.explode("theory_chunks")
      var exclude_session_groups: List[String] = Nil
      var all_sessions = false
      var dirs: List[Path] = Nil
      var session_groups: List[String] = Nil
      var options = Options.init()
      var exclude_sessions: List[String] = Nil

      val getopts = Getopts("""
Usage: isabelle theory_chunks [OPTIONS] [SESSIONS ...]

  Options are:
      -B NAME      include session NAME and all descendants
      -D DIR       include session directory and select its sessions
      -I DIR       input dir
      -R           refer to requirements of selected sessions
      -O DIR       output dir
      -X NAME      exclude sessions from group NAME and all descendants
      -a           select all sessions
      -d DIR       include session directory
      -g NAME      select session group NAME
      -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)
      -x NAME      exclude session NAME and all descendants

""",
        "B:" -> (arg => base_sessions = base_sessions ::: List(arg)),
        "D:" -> (arg => select_dirs = select_dirs ::: List(Path.explode(arg))),
        "I:" -> (arg => input_dir = Path.explode(arg)),
        "R" -> (_ => requirements = true),
        "O:" -> (arg => output_dir = Path.explode(arg)),
        "X:" -> (arg => exclude_session_groups = exclude_session_groups ::: List(arg)),
        "a" -> (_ => all_sessions = true),
        "d:" -> (arg => dirs = dirs ::: List(Path.explode(arg))),
        "g:" -> (arg => session_groups = session_groups ::: List(arg)),
        "o:" -> (arg => options = options + arg),
        "x:" -> (arg => exclude_sessions = exclude_sessions ::: List(arg)))

      val sessions = getopts(args)

      val progress = new Console_Progress()

      theory_chunks(options,
        input_dir = input_dir,
        output_dir = output_dir,
        dirs = dirs,
        select_dirs = select_dirs,
        selection = Sessions.Selection(
          requirements = requirements,
          all_sessions = all_sessions,
          base_sessions = base_sessions,
          exclude_session_groups = exclude_session_groups,
          exclude_sessions = exclude_sessions,
          session_groups = session_groups,
          sessions = sessions),
        progress = progress)
    })

  val evaluate_isabelle_tool = Isabelle_Tool("theory_chunks_evaluate", "evaluate chunk results",
    Scala_Project.here,
    { args =>
      var chunks_dir = Path.explode("theory_chunks")

      val getopts = Getopts("""
Usage: isabelle theory_chunks_eval [OPTIONS]

  Options are:
      -C DIR       chunks dir
""",
        "C:" -> (arg => chunks_dir = Path.explode(arg)))

      getopts(args)

      val progress = new Console_Progress()

      val pred = split_lines(File.read_gzip(chunks_dir + Path.basic("quality_pred.gz")))
      val thys = split_lines(File.read_gzip(chunks_dir + Path.basic("thy_names.gz")))

      val Line = """(\d+).*""".r

      val pred_scores = pred.map {
        case Line(num) => Some(Value.Int.parse(num))
        case s =>
          progress.echo("Could not parse pred " + quote(s))
          None
      }

      val res = thys.zip(pred_scores).groupBy(_._1).toList.map {
        case (thy, scores) =>
          val sc = scores.toList.map(_._2).flatten
          CSV.Record(thy, sc.sum.toDouble / sc.length)
      }

      CSV.File("theory_quality_pred", List("theory,score"), res).write(chunks_dir)
    })
}