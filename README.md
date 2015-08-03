# hrun
Hrun is a quick and dirty simple Haskell program that runs a program or shell command and returns various
data about it, including exit code, run time, and a few other goodies. 
It replicates some of the functionality of the Unix shell command `time` (typically not present on Windows).

    usage: hrun <options> <exec> <arg1> <arg2> ... <argn>
    where <options> are:
      -c     color stderr
      -s     treat the arguments as a shell command
      -t     print run times

## Observing exit codes
Without any options `hrun` will display the exit code of the command run.
For example,

    % hrun dir 

executes `dir` and appends the exit code (in green for success, and red for failure).

## Observing run time
Hrun will provide a crude estimiate in wall time for how long a command takes to run if
passed the `-t` option.  For example, the following command prints the following output. 

    % hrun -t sleep 5
    exited: 0
    time:   5.407 s

## Coloring stderr
The `-c` option will cause stderr of the child process to be colored red.  
This allows the user to distinguish what's coming from stderr and what's stdout.

## Execute as shell arguments
Shell commands require the command to be executed as a command.  The `-s` provides this capability.

    hrun -s "dir *.exe"
