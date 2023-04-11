package isabelle


import isabelle.*
import isabelle.utils.Lines


object Change_Frequencies {

  def change_frequencies(
    change_fac: Double = 0.5,
    version: String = "Isabelle2022",
    options: Options,
    output_dir: Path,
    dirs: List[Path],
    select_dirs: List[Path],
    selection: Sessions.Selection,
    progress: Progress = new Progress
  ): Unit = {
    Isabelle_System.make_directory(output_dir)

    val full_sessions = Sessions.load_structure(options, dirs, select_dirs)
    val deps = full_sessions.selection_deps(selection, progress, verbose = true)
    val sessions = deps.sessions_structure.build_selection(selection)

    val Changes = """ \d+ files changed, (\d+) insertions\(\+\), (\d+) deletions\(-\)""".r
    val Changeset_Date = """date: \s*(.*)""".r
    val Changeset_Id = """changeset: \s*(.*)""".r
    val Churn = """ thys/(\S+) \s*\|\s*(\d+) .*""".r

    val afp_path = Path.explode("$AFP/..")
    val afp_repo = Mercurial.the_repository(afp_path)

    val date_fmt = Date.Format("EEE MMM dd HH:mm:ss yyyy Z")
    val afp_date =
      afp_repo.command("log", "-r " + version).check.out_lines.collectFirst {
        case Changeset_Date(str) => date_fmt.parse(str)
      }.get

    var cache = Map.empty[String, (Set[String], List[(String, Int)])]
    val Thy_Line = """ thys/([^/]+)/.*""".r
    def entries_affected(id: String): (Set[String], List[(String, Int)]) =
      cache.getOrElse(id, {
        val res_lines = afp_repo.command("log", "--stat -r " + id).check.out_lines
        val churn = res_lines.collect {
          case Churn(file, chgs) => (file, Value.Int.parse(chgs))
        }
        val entries = res_lines.collect { case Thy_Line(name) => name}.toSet
        if (entries.isEmpty) error("Empty for " + id)
        if (churn.isEmpty) error("No churn for " + id)
        cache += id -> (entries, churn)
        (entries, churn)
      })

    val res =
      for {
        session_name <- sessions
        thy_node <- deps(session_name).proper_session_theories
        if thy_node.path.absolute.implode.startsWith(afp_path.absolute.implode)
      } yield {
        val thys_dir = afp_path.absolute.implode + "/thys/"
        val rel_path = thy_node.path.absolute.implode.stripPrefix(thys_dir)
        val name = rel_path.split("/").head
        progress.echo("Processing " + thy_node.theory + " in " + name)
        val res = afp_repo.command("log", "--no-merges --follow --stat " + thy_node.path).check
        val changesets = res.out_lines.collect { case Changeset_Id(id) => id }

        val since = res.out_lines.collect { case Changeset_Date(str) => date_fmt.parse(str) }.last

        val affected_entries = changesets.map(entries_affected(_))
        val (single, multiple) = affected_entries.partition(_._1 == Set(name))

        val time_days = (afp_date.unix_epoch - since.unix_epoch) / (60 * 60 * 24)
        val num_multiple = multiple.length
        val multiple_churn = multiple.flatMap(_._2).filter(_._1 == rel_path).map(_._2).sum
        val num_single = single.length
        val single_churn = single.flatMap(_._2).filter(_._1 == rel_path).map(_._2).sum
        val record =
          CSV.Record(session_name, thy_node.theory, time_days, num_multiple, num_single,
            multiple_churn, single_churn)
        progress.echo(record.toString)
        record
      }

    val header = List("session", "theory", "time_days", "multiple", "single", "churn_multiple", "churn_single")
    CSV.File("change_frequencies", header, res).write(output_dir)
  }


  /* isabelle tools */


  val isabelle_tool = Isabelle_Tool("change_frequencies", "compute change frequency for theories",
    Scala_Project.here,
    { args =>
      var base_sessions: List[String] = Nil
      var select_dirs: List[Path] = Nil
      var requirements = false
      var output_dir = Path.explode("data")
      var exclude_session_groups: List[String] = Nil
      var all_sessions = false
      var dirs: List[Path] = Nil
      var change_fac = 0.5
      var session_groups: List[String] = Nil
      var options = Options.init()
      var exclude_sessions: List[String] = Nil

      val getopts = Getopts("""
Usage: isabelle change_frequencies [OPTIONS] [SESSIONS ...]

  Options are:
      -B NAME      include session NAME and all descendants
      -D DIR       include session directory and select its sessions
      -R           refer to requirements of selected sessions
      -O DIR       output dir
      -X NAME      exclude sessions from group NAME and all descendants
      -a           select all sessions
      -d DIR       include session directory
      -f NUM       how many deletions per addition need to be in a changeset before it is counted
      -g NAME      select session group NAME
      -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)
      -x NAME      exclude session NAME and all descendants

""",
        "B:" -> (arg => base_sessions = base_sessions ::: List(arg)),
        "D:" -> (arg => select_dirs = select_dirs ::: List(Path.explode(arg))),
        "R" -> (_ => requirements = true),
        "O:" -> (arg => output_dir = Path.explode(arg)),
        "X:" -> (arg => exclude_session_groups = exclude_session_groups ::: List(arg)),
        "a" -> (_ => all_sessions = true),
        "d:" -> (arg => dirs = dirs ::: List(Path.explode(arg))),
        "f:" -> (arg => change_fac = Value.Int.parse(arg)),
        "g:" -> (arg => session_groups = session_groups ::: List(arg)),
        "o:" -> (arg => options = options + arg),
        "x:" -> (arg => exclude_sessions = exclude_sessions ::: List(arg)))

      val sessions = getopts(args)

      val progress = new Console_Progress()

      change_frequencies(
        options = options,
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
}