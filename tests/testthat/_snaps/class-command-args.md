# descriptions snaps

    Code
      ca$help()
    Output
      {scribe} command_args
      
      file : {path}
      
      DESCRIPTION
        First part here.  Followed by a second sentences.
      
      USAGE
        {command} [--help | --version]
        {command} 
      
      ARGUMENTS
        --help    prints this and quietly exits                   
        --version prints the version of {scribe} and quietly exits

---

    Code
      ca$help()
    Output
      {scribe} command_args
      
      file : {path}
      
      DESCRIPTION
        First part here.  Followed by a second sentences.
      
        A new line should be appended.
      
      USAGE
        {command} [--help | --version]
        {command} 
      
      ARGUMENTS
        --help    prints this and quietly exits                   
        --version prints the version of {scribe} and quietly exits

---

    Code
      ca$help()
    Output
      {scribe} command_args
      
      file : {path}
      
      DESCRIPTION
        description
      
      USAGE
        {command} [--help | --version]
        {command} 
      
      ARGUMENTS
        --help    prints this and quietly exits                   
        --version prints the version of {scribe} and quietly exits

---

    Code
      ca$help()
    Output
      {scribe} command_args
      
      file : {path}
      
      DESCRIPTION
        NA
      
      USAGE
        {command} [--help | --version]
        {command} 
      
      ARGUMENTS
        --help    prints this and quietly exits                   
        --version prints the version of {scribe} and quietly exits

# examples snaps

    Code
      ca$help()
    Output
      {scribe} command_args
      
      file : {path}
      
      USAGE
        {command} [--help | --version]
        {command} 
      
      ARGUMENTS
        --help    prints this and quietly exits                   
        --version prints the version of {scribe} and quietly exits
      
      EXAMPLES
        $ foo --flag      
        $ foo --other-flag

---

    Code
      ca$help()
    Output
      {scribe} command_args
      
      file : {path}
      
      USAGE
        {command} [--help | --version]
        {command} 
      
      ARGUMENTS
        --help    prints this and quietly exits                   
        --version prints the version of {scribe} and quietly exits

---

    Code
      ca$help()
    Output
      {scribe} command_args
      
      file : {path}
      
      USAGE
        {command} [--help | --version]
        {command} 
      
      ARGUMENTS
        --help    prints this and quietly exits                   
        --version prints the version of {scribe} and quietly exits
      
      EXAMPLES
        foo command value

# snapshots

    Code
      ca$show()
    Output
      Initial call:  foo bar --fizz
      w Call $resolve() or $parse() to resolve arguments
      Argument [--help] : FALSE
      Argument [--version] : FALSE

---

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
        --help    prints this and quietly exits                   
        --version prints the version of {scribe} and quietly exits

# snapshots - empty values

    Code
      ca$get_args()
    Output
      [[1]]
      Argument [--help] R : FALSE
      
      [[2]]
      Argument [--version] R : FALSE
      
      [[3]]
      Argument [--foo] R : <null>
      
      [[4]]
      Argument [--bar] R : zero
      
      [[5]]
      Argument [--fizz] R : <empty>
      

# snapshots - super args

    Code
      command_args("---help")$parse()
    Output
      {scribe} v0.3.0.9001
      For more information, see https://jmbarbone.github.io/scribe/
      named list()

---

    Code
      command_args("---version")$parse()
    Output
      0.3.0.9001
      named list()

