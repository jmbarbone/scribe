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
        --help    : prints this and quietly exits                   
        --version : prints the version of {scribe} and quietly exits

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
        --help    : prints this and quietly exits                   
        --version : prints the version of {scribe} and quietly exits

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
        --help    : prints this and quietly exits                   
        --version : prints the version of {scribe} and quietly exits

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
        --help    : prints this and quietly exits                   
        --version : prints the version of {scribe} and quietly exits

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
        --help    : prints this and quietly exits                   
        --version : prints the version of {scribe} and quietly exits
      
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
        --help    : prints this and quietly exits                   
        --version : prints the version of {scribe} and quietly exits

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
        --help    : prints this and quietly exits                   
        --version : prints the version of {scribe} and quietly exits
      
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
        --help    : prints this and quietly exits                   
        --version : prints the version of {scribe} and quietly exits

