type ('column, 'row, 'color) t

val create : unit -> ('column, 'row, 'color) t
val add_primary : ('column, 'row, 'color) t -> 'column -> unit
val add_secondary : ('column, 'row, 'color) t -> 'column -> unit
val add_colored : ('column, 'row, 'color) t -> 'column -> unit
val add_shape : ('column, 'row, 'color) t -> 'row -> 'column list -> unit
val get_cols_of_row : ('column, 'row, 'color) t -> 'row -> 'column list
val get_cols : ('column, 'row, 'color) t -> 'column Seq.t
val get_primary_cols : ('column, 'row, 'color) t -> 'column Seq.t
val get_rows : ('column, 'row, 'color) t -> 'row Seq.t
val get_primary_cols_of_row : ('column, 'row, 'color) t -> 'row -> 'column list
val get_rows_of_col : ('column, 'row, 'color) t -> 'column -> 'row list
val get_rows_of_row : ('column, 'row, 'color) t -> 'row -> 'row list
