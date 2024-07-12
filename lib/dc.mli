type ('a, 'b) t

val make : ('a, 'b) Ec.t -> ('a, 'b) t
val count_solutions : ('a, 'b) t -> int
