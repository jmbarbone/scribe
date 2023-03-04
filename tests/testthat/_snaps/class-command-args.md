# snapshots

    Code
      ca$help()
    Output
      {scribe} command_args
      
      file : {path}
      
      DESCRIPTION
        this does a thing
      
      USAGE
        {command} [--help | --version]
        {command}  
      
      ARGUMENTS
        --help    : prints this and quietly exits                   
        --version : prints the version of {scribe} and quietly exits

---

    Code
      ca$show()
    Output
      Initial call:  foo bar --fizz
      w Call $resolve() or $parse() to resolve arguments
      Argument [--help] : FALSE
      Argument [--version] : FALSE

