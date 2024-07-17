#include <assert.h>
#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <stdbool.h>
#include <stdint.h>

struct node {
  struct node *next;
};

typedef struct node *ptr;

inline void hide_cr(ptr col, ptr cr_pos) {
  register ptr rc_pos = cr_pos->next;
  register ptr row = (cr_pos + 1)->next;
  register ptr rc_end = (row - 1)->next + 2;
  (row - 1)->next = rc_end;
  if (rc_pos > rc_end) {
    register ptr cr_pos2 = rc_end->next;
    register ptr col2 = (rc_end + 1)->next;
    (rc_pos)->next = cr_pos2;
    (rc_pos + 1)->next = col2;
    (rc_end)->next = cr_pos;
    (rc_end + 1)->next = col;
    (cr_pos)->next = rc_end;
    (cr_pos2)->next = rc_pos;
  }
}

inline void unhide_cr(ptr cr_pos) {
  register ptr row = (cr_pos + 1)->next;
  (row - 1)->next -= 2;
}

inline void hide_rc(ptr row, ptr rc_pos) {
  register ptr cr_pos = (rc_pos)->next;
  register ptr col = (rc_pos + 1)->next;
  register ptr cr_end = (col)->next - 2;
  (col)->next = cr_end;
  if (cr_pos < cr_end) {
    register ptr rc_pos2 = (cr_end)->next;
    register ptr row2 = (cr_end + 1)->next;
    (cr_pos)->next = rc_pos2;
    (cr_pos + 1)->next = row2;
    (cr_end)->next = rc_pos;
    (cr_end + 1)->next = row;
    (rc_pos)->next = cr_end;
    (rc_pos2)->next = cr_pos;
  }
}

inline void unhide_rc(ptr rc_pos) {
  register ptr col = (rc_pos + 1)->next;
  (col)->next += 2;
}

inline void hide_rr(ptr row, ptr rr_pos) {
  register ptr rr_sop = (rr_pos)->next;
  register ptr wor = (rr_pos + 1)->next;
  register ptr rr_dne = (wor)->next - 2;
  (wor)->next = rr_dne;
  if (rr_sop < rr_dne) {
    register ptr rr_pos2 = (rr_dne)->next;
    register ptr row2 = (rr_dne + 1)->next;
    (rr_sop)->next = rr_pos2;
    (rr_sop + 1)->next = row2;
    (rr_dne)->next = rr_pos;
    (rr_dne + 1)->next = row;
    (rr_pos)->next = rr_dne;
    (rr_pos2)->next = rr_sop;
  }
}

inline void unhide_rr(ptr rr_pos) {
  register ptr wor = (rr_pos + 1)->next;
  (wor)->next += 2;
}

inline void disable_col(ptr col) {
  register ptr cr_pos = col + 1;
  register ptr cr_end = (col)->next;
  while (cr_pos < cr_end) {
    hide_cr(col, cr_pos);
    cr_pos += 2;
  }
}

inline void enable_col(ptr col) {
  register ptr cr_pos = (col)->next - 2;
  while (cr_pos > col) {
    unhide_cr(cr_pos);
    cr_pos -= 2;
  }
}

inline void disable_row(ptr row) {
  {
    register ptr rc_pos = row - 3;
    register ptr rc_end = (row - 1)->next;
    while (rc_pos > rc_end) {
      hide_rc(row, rc_pos);
      rc_pos -= 2;
    }
  }
  {
    register ptr rr_pos = row + 1;
    register ptr rr_end = (row)->next;
    while (rr_pos < rr_end) {
      hide_rr(row, rr_pos);
      rr_pos += 2;
    }
  }
}

inline void enable_row(ptr row) {
  {
    register ptr rc_pos = (row - 1)->next + 2;
    while (rc_pos < row - 1) {
      unhide_rc(rc_pos);
      rc_pos += 2;
    }
  }
  {
    register ptr rr_pos = (row)->next - 2;
    while (rr_pos > row) {
      unhide_rr(rr_pos);
      rr_pos -= 2;
    }
  }
}

inline void move_to_head(ptr arr, ptr col, ptr col_pos) {
  register ptr head = arr->next;
  if (col_pos > head) {
    ptr col2 = (head)->next;
    (head)->next = col;
    (col - 1)->next = head;
    (col_pos)->next = col2;
    (col2 - 1)->next = col_pos;
  }
  arr->next = head + 2;
}

inline void remove_from_head(ptr arr) { arr->next -= 2; }

inline void move_to_tail(ptr arr, ptr col, ptr col_pos) {
  register ptr tail = (arr + 1)->next - 2;
  (arr + 1)->next = tail;
  if (col_pos < tail) {
    register ptr col2 = (tail)->next;
    (tail)->next = col;
    (col_pos)->next = col2;
    (col - 1)->next = tail;
    (col2 - 1)->next = col_pos;
  }
}

inline void remove_from_tail(ptr arr) { (arr + 1)->next += 2; }

inline void select_row(ptr arr, ptr row) {
  disable_row(row);
  register ptr col_ptr = row - 3;
  register ptr col_end = (row - 1)->next;
  while (col_ptr > col_end) {
    ptr col = (col_ptr + 1)->next;
    ptr col_pos = (col - 1)->next;
    move_to_tail(arr, col, col_pos);
    col_ptr -= 2;
  }
  register ptr row_ptr = row + 1;
  register ptr row_end = (row)->next;
  while (row_ptr < row_end) {
    disable_row((row_ptr + 1)->next);
    row_ptr += 2;
  }
}

inline void deselect_row(ptr arr, ptr row) {
  register ptr row_ptr = (row)->next - 2;
  while (row_ptr > row) {
    enable_row((row_ptr + 1)->next);
    row_ptr -= 2;
  }
  register ptr col_ptr = (row - 1)->next + 2;
  while (col_ptr < row - 1) {
    remove_from_tail(arr);
    col_ptr += 2;
  }
  enable_row(row);
}

inline void cover_col(ptr arr, ptr col) {
  disable_col(col);
  move_to_head(arr, col, (col - 1)->next);
}

inline void uncover_col(ptr arr, ptr col) {
  remove_from_head(arr);
  enable_col(col);
}

void _mix(ptr arr, bool forward) {
  if (forward)
    goto move_in;
move_out: {
  ptr head = (arr)->next;
  if (head <= arr + 2)
    return;
  ptr cr_pos = (head - 1)->next;
  deselect_row(arr, (cr_pos + 1)->next);
  (head - 1)->next = cr_pos + 2;
}
move_on: {
  ptr head = (arr)->next;
  ptr col = (head - 2)->next;
  ptr cr_pos = (head - 1)->next;
  if (cr_pos >= (col)->next) {
    uncover_col(arr, col);
    goto move_out;
  } else {
    select_row(arr, (cr_pos + 1)->next);
  }
}
move_in: {
  ptr head = (arr)->next;
  ptr tail = (arr + 1)->next;
  if (head < tail) {
    ptr cnd_pos = head;
    ptr cnd_col = (cnd_pos)->next;
    ptr cnd_card = (ptr)((uint64_t)(cnd_col)->next - (uint64_t)cnd_col);
    ptr cur_pos = head + 2;
    while (cur_pos < tail) {
      ptr cur_col = (cur_pos)->next;
      ptr cur_card = (ptr)((uint64_t)(cur_col)->next - (uint64_t)cur_col);
      if (cur_card < cnd_card) {
        cnd_pos = cur_pos;
        cnd_col = cur_col;
        cnd_card = cur_card;
      }
      cur_pos += 2;
    }
    cover_col(arr, cnd_col);
    (head + 1)->next = cnd_col + 1;
    goto move_on;
  }
}
}

CAMLprim value forward(value bigarray) {
  ptr arr = Caml_ba_data_val(bigarray);
  // _forward(arr);
  // for (int i = 0; i < 20; i++) {
  //   printf("%llu : %llu\n", (uint64_t)(arr + i), (uint64_t)(arr + i)->next);
  // }
  // printf("\n%llu : %llu\n", (uint64_t)(arr->next), (uint64_t)(arr + 2));
  // assert(arr->next == arr + 2);
  _mix(arr, true);
  return Val_unit;
}

CAMLprim value backward(value bigarray) {
  ptr arr = Caml_ba_data_val(bigarray);
  // _backward(arr);
  _mix(arr, false);
  return Val_unit;
}
