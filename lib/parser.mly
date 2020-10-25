%token <string> ARG
%token PIPE
%token EOF

%start <string list list option> start
%%

start:
  | EOF
    { None }
  | command; EOF
    { Some $1 }
  ;

command:
  | from = cmd; PIPE; into = command
    { from :: into }
  | cmd
    { [$1] }
  ;

cmd:
  | ARG+
    { $1 }
  ;