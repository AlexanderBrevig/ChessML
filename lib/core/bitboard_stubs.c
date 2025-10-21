/* Fast bitboard operations using CPU intrinsics */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <stdint.h>

/* Count trailing zeros (find LSB position) using CPU intrinsic
 * This compiles to a single bsfq instruction on x86-64
 * Returns the bit position (0-63) of the least significant set bit
 * Returns -1 for empty bitboard
 */
CAMLprim value caml_bitboard_ctz(value v_bb) {
  uint64_t bb = (uint64_t)Int64_val(v_bb);
  
  if (bb == 0) {
    return Val_int(-1);  /* Return -1 for empty bitboard */
  }
  
#if defined(__GNUC__) || defined(__clang__)
  /* Use GCC/Clang builtin - compiles to bsfq/ctzq instruction */
  int pos = __builtin_ctzll(bb);
  return Val_int(pos);
#elif defined(_MSC_VER)
  /* Use MSVC intrinsic */
  unsigned long pos;
  _BitScanForward64(&pos, bb);
  return Val_int((int)pos);
#else
  /* Fallback: software implementation using De Bruijn multiplication
   * This is still much faster than the OCaml loop
   */
  static const int debruijn64[64] = {
    0,  47,  1, 56, 48, 27,  2, 60,
    57, 49, 41, 37, 28, 16,  3, 61,
    54, 58, 35, 52, 50, 42, 21, 44,
    38, 32, 29, 23, 17, 11,  4, 62,
    46, 55, 26, 59, 40, 36, 15, 53,
    34, 51, 20, 43, 31, 22, 10, 45,
    25, 39, 14, 33, 19, 30,  9, 24,
    13, 18,  8, 12,  7,  6,  5, 63
  };
  
  /* Isolate the LSB */
  uint64_t lsb = bb & -bb;
  
  /* De Bruijn multiplication to get bit position */
  int pos = debruijn64[((lsb * 0x03f79d71b4cb0a89ULL) >> 58) & 63];
  return Val_int(pos);
#endif
}
