
-type headers() :: [{bitstring(), bitstring()}].
-type item() :: {integer(), headers(), bitstring()}.
-type state() :: #{ bitstring() => [item()]}.
