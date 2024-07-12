type ('col, 'row) t

val create : unit -> ('col, 'row) t
val add_primary : ('col, 'row) t -> 'col -> unit
val add_secondary : ('col, 'row) t -> 'col -> unit
val add_shape : ('col, 'row) t -> 'row -> 'col list -> unit
val get_cols_of_row : ('col, 'row) t -> 'row -> 'col list
val get_cols : ('col, 'row) t -> 'col Seq.t
val get_primary_cols : ('col, 'row) t -> 'col Seq.t
val get_rows : ('col, 'row) t -> 'row Seq.t
val get_primary_cols_of_row : ('col, 'row) t -> 'row -> 'col list
val get_rows_of_col : ('col, 'row) t -> 'col -> 'row list
val get_rows_of_row : ('col, 'row) t -> 'row -> 'row list
