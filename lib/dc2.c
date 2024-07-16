#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <stdbool.h>
#include <stdint.h>

inline void hide_cr(int32_t *arr, int32_t col, int32_t cr_pos) {
  register int32_t rc_pos = arr[cr_pos];
  register int32_t row = arr[cr_pos + 1];
  register int32_t rc_end = arr[row - 1] + 2;
  arr[row - 1] = rc_end;
  if (rc_pos > rc_end) {
    register int32_t cr_pos2 = arr[rc_end];
    register int32_t col2 = arr[rc_end + 1];
    arr[rc_pos] = cr_pos2;
    arr[rc_pos + 1] = col2;
    arr[rc_end] = cr_pos;
    arr[rc_end + 1] = col;
    arr[cr_pos] = rc_end;
    arr[cr_pos2] = rc_pos;
  }
}

inline void unhide_cr(int32_t *arr, int32_t cr_pos) {
  int32_t row = arr[cr_pos + 1];
  arr[row - 1] -= 2;
}

inline void hide_rc(int32_t *arr, int32_t row, int32_t rc_pos) {
  register int32_t cr_pos = arr[rc_pos];
  register int32_t col = arr[rc_pos + 1];
  register int32_t cr_end = arr[col] - 2;
  arr[col] = cr_end;
  if (cr_pos < cr_end) {
    register int32_t rc_pos2 = arr[cr_end];
    register int32_t row2 = arr[cr_end + 1];
    arr[cr_pos] = rc_pos2;
    arr[cr_pos + 1] = row2;
    arr[cr_end] = rc_pos;
    arr[cr_end + 1] = row;
    arr[rc_pos] = cr_end;
    arr[rc_pos2] = cr_pos;
  }
}

inline void unhide_rc(int32_t *arr, int32_t rc_pos) {
  register int32_t col = arr[rc_pos + 1];
  arr[col] += 2;
}

inline void hide_rr(int32_t *arr, int32_t row, int32_t rr_pos) {
  register int32_t rr_sop = arr[rr_pos];
  register int32_t wor = arr[rr_pos + 1];
  register int32_t rr_dne = arr[wor] - 2;
  arr[wor] = rr_dne;
  if (rr_sop < rr_dne) {
    register int32_t rr_pos2 = arr[rr_dne];
    register int32_t row2 = arr[rr_dne + 1];
    arr[rr_sop] = rr_pos2;
    arr[rr_sop + 1] = row2;
    arr[rr_dne] = rr_pos;
    arr[rr_dne + 1] = row;
    arr[rr_pos] = rr_dne;
    arr[rr_pos2] = rr_sop;
  }
}

inline void unhide_rr(int32_t *arr, int32_t rr_pos) {
  register int32_t wor = arr[rr_pos + 1];
  arr[wor] += 2;
}

inline void disable_col(int32_t *arr, int32_t col) {
  register int32_t cr_pos = col + 1;
  register int32_t cr_end = arr[col];
  while (cr_pos < cr_end) {
    hide_cr(arr, col, cr_pos);
    cr_pos += 2;
  }
}

inline void enable_col(int32_t *arr, int32_t col) {
  register int32_t cr_pos = arr[col] - 2;
  while (cr_pos > col) {
    unhide_cr(arr, cr_pos);
    cr_pos -= 2;
  }
}

inline void disable_row(int32_t *arr, int32_t row) {
  {
    register int32_t rc_pos = row - 3;
    register int32_t rc_end = arr[row - 1];
    while (rc_pos > rc_end) {
      hide_rc(arr, row, rc_pos);
      rc_pos -= 2;
    }
  }
  {
    register int32_t rr_pos = row + 1;
    register int32_t rr_end = arr[row];
    while (rr_pos < rr_end) {
      hide_rr(arr, row, rr_pos);
      rr_pos += 2;
    }
  }
}

inline void enable_row(int32_t *arr, int32_t row) {
  {
    register int32_t rc_pos = arr[row - 1] + 2;
    while (rc_pos < row - 1) {
      unhide_rc(arr, rc_pos);
      rc_pos += 2;
    }
  }
  {
    register int32_t rr_pos = arr[row] - 2;
    while (rr_pos > row) {
      unhide_rr(arr, rr_pos);
      rr_pos -= 2;
    }
  }
}

void move_to_head(int32_t *arr, int32_t col, int32_t col_pos) {
  int32_t head = arr[0];
  if (col_pos > head) {
    int32_t col2 = arr[head];
    arr[head] = col;
    arr[col - 1] = head;
    arr[col_pos] = col2;
    arr[col2 - 1] = col_pos;
  }
  arr[0] = head + 2;
}

void remove_from_head(int32_t *arr) { arr[0] -= 2; }

void move_to_tail(int32_t *arr, int32_t col, int32_t col_pos) {
  int32_t tail = arr[1] - 2;
  arr[1] = tail;
  if (col_pos < tail) {
    int32_t col2 = arr[tail];
    arr[tail] = col;
    arr[col_pos] = col2;
    arr[col - 1] = tail;
    arr[col2 - 1] = col_pos;
  }
}

void remove_from_tail(int32_t *arr) { arr[1] += 2; }

void select_row(int32_t *arr, int32_t row) {
  disable_row(arr, row);
  int32_t col_ptr = row - 3;
  int32_t col_end = arr[row - 1];
  while (col_ptr > col_end) {
    int32_t col = arr[col_ptr + 1];
    int32_t col_pos = arr[col - 1];
    move_to_tail(arr, col, col_pos);
    col_ptr -= 2;
  }
  int32_t row_ptr = row + 1;
  int32_t row_end = arr[row];
  while (row_ptr < row_end) {
    disable_row(arr, arr[row_ptr + 1]);
    row_ptr += 2;
  }
}

void deselect_row(int32_t *arr, int32_t row) {
  int32_t row_ptr = arr[row] - 2;
  while (row_ptr > row) {
    enable_row(arr, arr[row_ptr + 1]);
    row_ptr -= 2;
  }
  int32_t col_ptr = arr[row - 1] + 2;
  while (col_ptr < row - 1) {
    remove_from_tail(arr);
    col_ptr += 2;
  }
  enable_row(arr, row);
}

void cover_col(int32_t *arr, int32_t col) {
  disable_col(arr, col);
  move_to_head(arr, col, arr[col - 1]);
}

void uncover_col(int32_t *arr, int32_t col) {
  remove_from_head(arr);
  enable_col(arr, col);
}

void _mix(int32_t *arr, bool forward) {
  if (forward)
    goto move_in;
move_out: {
  int32_t head = arr[0];
  if (head <= 2)
    return;
  int32_t cr_pos = arr[head - 1];
  deselect_row(arr, arr[cr_pos + 1]);
  arr[head - 1] = cr_pos + 2;
}
move_on: {
  int32_t head = arr[0];
  int32_t col = arr[head - 2];
  int32_t cr_pos = arr[head - 1];
  if (cr_pos >= arr[col]) {
    uncover_col(arr, col);
    goto move_out;
  } else {
    select_row(arr, arr[cr_pos + 1]);
  }
}
move_in: {
  int32_t head = arr[0];
  int32_t tail = arr[1];
  if (head < tail) {
    int32_t cnd_pos = head;
    int32_t cnd_col = arr[cnd_pos];
    int32_t cnd_card = arr[cnd_col] - cnd_col;
    int32_t cur_pos = head + 2;
    while (cur_pos < tail) {
      int32_t cur_col = arr[cur_pos];
      int32_t cur_card = arr[cur_col] - cur_col;
      if (cur_card < cnd_card) {
        cnd_pos = cur_pos;
        cnd_col = cur_col;
        cnd_card = cur_card;
      }
      cur_pos += 2;
    }
    cover_col(arr, cnd_col);
    arr[head + 1] = cnd_col + 1;
    goto move_on;
  }
}
}

CAMLprim value forward(value bigarray) {
  int32_t *arr = Caml_ba_data_val(bigarray);
  // _forward(arr);
  _mix(arr, true);
  return Val_unit;
}

CAMLprim value backward(value bigarray) {
  int32_t *arr = Caml_ba_data_val(bigarray);
  // _backward(arr);
  _mix(arr, false);
  return Val_unit;
}
