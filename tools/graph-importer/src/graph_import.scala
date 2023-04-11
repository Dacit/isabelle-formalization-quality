package isabelle.graph


import isabelle.*
import Importer_Service.Import
import isabelle.graph.cypher.{Neo4j_Data_Repository, Cypher_Graph_Repository}
import org.neo4j.driver.AuthTokens


object Graph_Import {
  case class Component(name: String, imports: List[Import])

  def graph_import(
    components: List[Component],
    repo: Graph_Repository,
    fully_connected: Boolean = false,
    progress: Progress = new Progress
  ): Unit = {
    def import_sessions(imports: List[Import]): Unit = {
      progress.echo("importing " + imports.size + " sessions with " +
        imports.flatMap(_.theory_names).size + " theories...")
      try {
        val service = new Importer_Service(repo)
        service.import_sessions(imports)
        progress.echo("finished importing")
      } catch {
        case e: Exception => progress.echo_error_message(e.getMessage + "\n" +
          e.getStackTrace.mkString("\n"))
      }
    }

    if (fully_connected) {
      progress.echo("Building full graph...")
      import_sessions(components.flatMap(_.imports))
    } else {
      components.foreach { component =>
        progress.echo("Processing " + component.name + " (" + component.imports.size + " sessions)")
        import_sessions(component.imports)
      }
    }
  }

  def exported_theories(
    session_name: String, deps: Sessions.Deps,
    store: Sessions.Store
  ): List[String] = {
    val exported = using(Export.open_session_context0(store, session_name)) { _.theory_names() }
    val session_theories = deps(session_name).proper_session_theories.map(_.theory)
    session_theories.filter(exported.contains)
  }


  /* Isabelle Tool Wrapper */

  val isabelle_tool: Isabelle_Tool = Isabelle_Tool(
    "graph_import", "Import dependency graph", Scala_Project.here,
    args => {
      val build_options = Word.explode(Isabelle_System.getenv("ISABELLE_BUILD_OPTIONS"))

      var base_sessions: List[String] = Nil
      var fully_connected = false
      var select_dirs: List[Path] = Nil
      var numa_shuffling = false
      var requirements = false
      var verbose_build = false
      var exclude_session_groups: List[String] = Nil
      var all_sessions = false
      var clean_build = false
      var dirs: List[Path] = Nil
      var session_groups: List[String] = Nil
      var max_jobs = 1
      var n = 10
      var options = Options.init(opts = build_options) + "export_theory"
      var password = "admin"
      var import_requirements = false
      var exclude_sessions: List[String] = Nil

      val getopts = Getopts(
        """
Usage: isabelle graph_import [OPTIONS] GRAPHDB [SESSIONS...]

Options are:
-B NAME        include session NAME and all descendants
-C             build graphs connected between sessions
-D DIR         include session directory and select its sessions
-N             cyclic shuffling of NUMA CPU nodes (performance tuning)
-R             refer to requirements of selected sessions
-V             verbose build
-X NAME        exclude sessions from group NAME and all descendants
-a             select all sessions
-c             clean build
-d DIR         include session directory
-g NAME        select session group NAME
-j INT         maximum number of parallel jobs (default 1)
-o OPTION      override Isabelle system OPTION (via NAME=VAL or NAME)
-p PASSWORD    password for neo4j user 'neo4j'
-r             import requirements of selected sessions
-x NAME        exclude session NAME and all descendants

Analyze theorem dependency graph. Graph db must be host:port and accessible via neo4j user.
""",
        "B:" -> (arg => base_sessions = base_sessions ::: List(arg)),
        "C" -> (_ => fully_connected = true),
        "D:" -> (arg => select_dirs = select_dirs ::: List(Path.explode(arg))),
        "N" -> (_ => numa_shuffling = true),
        "R" -> (_ => requirements = true),
        "V" -> (_ => verbose_build = true),
        "X:" -> (arg => exclude_session_groups = exclude_session_groups ::: List(arg)),
        "a" -> (_ => all_sessions = true),
        "c" -> (_ => clean_build = true),
        "d:" -> (arg => dirs = dirs ::: List(Path.explode(arg))),
        "g:" -> (arg => session_groups = session_groups ::: List(arg)),
        "j:" -> (arg => max_jobs = Value.Int.parse(arg)),
        "n:" -> (arg => n = Value.Int.parse(arg)),
        "o:" -> (arg => options = options + arg),
        "p:" -> (arg => password = arg),
        "r" -> (_ => import_requirements = true),
        "x:" -> (arg => exclude_sessions = exclude_sessions ::: List(arg)))

      val (graphdb, sessions) = getopts(args) match {
        case graphdb :: sessions => (graphdb, sessions)
        case _ => getopts.usage()
      }

      val neo4j = Neo4j_Data_Repository.remote(graphdb, AuthTokens.basic("neo4j", password))

      val progress = new Console_Progress()

      val selection = Sessions.Selection(
        requirements = requirements,
        all_sessions = all_sessions,
        base_sessions = base_sessions,
        exclude_session_groups = exclude_session_groups,
        exclude_sessions = exclude_sessions,
        session_groups = session_groups,
        sessions = sessions)

      progress.interrupt_handler {
        val res =
          Build.build(
            options = options,
            selection = selection,
            progress = progress,
            clean_build = clean_build,
            dirs = dirs,
            select_dirs = select_dirs,
            numa_shuffling = NUMA.enabled_warning(progress, numa_shuffling),
            max_jobs = max_jobs,
            verbose = verbose_build)
        if (!res.ok) System.exit(res.rc)
      }

      val store = Sessions.store(options)

      val library_sessions = Sessions.load_structure(options).selection(
        Sessions.Selection(all_sessions = true)).imports_topological_order.toSet
      val full_sessions =
        Sessions.load_structure(options = options, dirs = dirs, select_dirs = select_dirs)

      val sessions_structure = full_sessions.selection(selection)
      val deps = Sessions.deps(sessions_structure)

      val selected_sessions = sessions_structure.build_selection(selection).toSet

      val trans_sessions =
        sessions_structure.selection(selection).build_topological_order.filter(_ != "Pure")

      val sessions1 =
        if (import_requirements) trans_sessions
        else trans_sessions.filter(selected_sessions.contains)

      val imports = sessions1.map { session_name =>

        val is_library = library_sessions.contains(session_name)
        val theories = exported_theories(session_name, deps, store)

        def provider = Export.open_session_context0(store, session_name)

        Import(is_library, session_name, () => provider, theories)
      }

      // one component per session - fully_connected controls deps between components
      val components = imports.map(i => Component(i.session_name, List(i)))

      using(Cypher_Graph_Repository(neo4j)) { repo =>
        graph_import(components = components, repo = repo, fully_connected = fully_connected,
          progress = progress)
      }
    })
}

class Tools extends Isabelle_Scala_Tools(Graph_Import.isabelle_tool)
