/* Bit scans and population count using compiler intrinsics.
 *
 * Each operation has a native-code entry point taking an unboxed int64 and
 * returning an untagged int (no allocation, no runtime call overhead) and a
 * bytecode entry point taking boxed values. */

#include <caml/mlvalues.h>
#include <stdint.h>

#if defined(__GNUC__) || defined(__clang__)
#define CTZ64(x) __builtin_ctzll(x)
#define CLZ64(x) __builtin_clzll(x)
#define POPCOUNT64(x) __builtin_popcountll(x)
#else
static int CTZ64(uint64_t x) { int n = 0; while (!(x & 1)) { x >>= 1; n++; } return n; }
static int CLZ64(uint64_t x) { int n = 0; while (!(x >> 63)) { x <<= 1; n++; } return n; }
static int POPCOUNT64(uint64_t x) { int n = 0; while (x) { x &= x - 1; n++; } return n; }
#endif

/* Index of the least significant set bit, -1 for an empty bitboard */
intnat caml_bitboard_ctz_unboxed(int64_t bb) {
  return bb == 0 ? -1 : CTZ64((uint64_t)bb);
}

value caml_bitboard_ctz(value v_bb) {
  return Val_long(caml_bitboard_ctz_unboxed(Int64_val(v_bb)));
}

/* Index of the most significant set bit, -1 for an empty bitboard */
intnat caml_bitboard_msb_unboxed(int64_t bb) {
  return bb == 0 ? -1 : 63 - CLZ64((uint64_t)bb);
}

value caml_bitboard_msb(value v_bb) {
  return Val_long(caml_bitboard_msb_unboxed(Int64_val(v_bb)));
}

/* Number of set bits */
intnat caml_bitboard_popcount_unboxed(int64_t bb) {
  return POPCOUNT64((uint64_t)bb);
}

value caml_bitboard_popcount(value v_bb) {
  return Val_long(caml_bitboard_popcount_unboxed(Int64_val(v_bb)));
}
