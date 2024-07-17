type ('column, 'row) t

val create : unit -> ('column, 'row) t
val add_primary : ('column, 'row) t -> 'column -> unit
val add_secondary : ('column, 'row) t -> 'column -> unit
val add_shape : ('column, 'row) t -> 'row -> 'column list -> unit
val get_cols_of_row : ('column, 'row) t -> 'row -> 'column list
val get_cols : ('column, 'row) t -> 'column Seq.t
val get_primary_cols : ('column, 'row) t -> 'column Seq.t
val get_rows : ('column, 'row) t -> 'row Seq.t
val get_primary_cols_of_row : ('column, 'row) t -> 'row -> 'column list
val get_rows_of_col : ('column, 'row) t -> 'column -> 'row list
val get_rows_of_row : ('column, 'row) t -> 'row -> 'row list
