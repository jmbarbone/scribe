test_that("gh example", {
  # this is based on the gh cli but doesn't actually need to execute anything
  cmd_version <- command_args()
  cmd_version$add_argument("-s", "--short", action = "flag")

  cmd_list <- command_args()
  cmd_list$add_argument("-a", "--all", action = "flag")
  cmd_list$add_argument("-l", "--long", action = "flag")

  main <- command_args()
  main$set_commands(
    version = cmd_version,
    list = cmd_list
  )

  gh <- command_args()
  gh_auth <- command()
  gh_auth_login <- command()
  gh_auth_login$add_argument("-w", "--web", action = "flag")
  gh_auth_login$add_argument("--with-token", action = "list")
  gh_auth_login$add_argument("-h", "--host-name", action = "list")
  gh_auth_logout <- command()
  gh_auth_logout$add_argument("-h", "--host-name", action = "list")
  gh_auth_logout$add_argument("-u", "--user", action = "list")

  gh_auth$set_commands(login = gh_auth_login, logout = gh_auth_logout)
  gh_pr <- command()
  gh_pr_create <- command()
  gh_pr_create$add_argument(scribeArg("-a", "--assignee", action = "list"))
  gh_pr_create$add_argument(scribeArg("-B", "--base", action = "list"))
  gh_pr$set_commands(
    create = gh_pr_create,
    list = scribeCommand(),
    status = scribeCommand()
  )

  args <- gh$parse()
  list(
    # all commands
    .commands = c("pr", "create"),
    assignee = "jmbarbone",
    base = "main"
  )
  succeed()

  # USAGE
  #   main [COMMAND] [ARGS]
  #   main list [-a,--all] [-l,--long]
  #   main version [-s,--short]
  #
  # COMMANDS
  #   version Prints out version
  #   list    Lists components
})
