/* QBE-backend runtime. Linked with the qbe-generated assembly.
   Mirrors orto's region model: a slab per Region, generation-checked
   Handles (use-after-free/reset aborts). Region = {long slot; long gen};
   Handle = {long slot; long off; long len; long gen}. All passed by
   pointer from the generated code (no aggregate ABI). */
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct { char *buf; long cap; long used; long gen; int live; } RtReg;
#define ORTO_MAXR 1048576
static RtReg RT[ORTO_MAXR];
static long  rt_top = 0;

static long reg_alloc(long n) {
  long s = rt_top++;
  if (s >= ORTO_MAXR) abort();
  RT[s].buf = (n > 0) ? (char *)malloc((size_t)n) : (char *)malloc(1);
  if (!RT[s].buf) abort();
  RT[s].cap = n; RT[s].used = 0; RT[s].gen += 1; RT[s].live = 1;
  return s;
}

/* out = {slot, gen} */
void orto_rt_region(long n, long *out) {
  long s = reg_alloc(n);
  out[0] = s; out[1] = RT[s].gen;
}

static void chk_reg(long *r) { if (!RT[r[0]].live || RT[r[0]].gen != r[1]) abort(); }
static void chk_h(long *h)   { if (!RT[h[0]].live || RT[h[0]].gen != h[3]) abort(); }

/* allocate `count` cells of `elemsize` bytes in region r, optionally
   filling each with the `elemsize` bytes at init (NULL = leave zeroed).
   out = Handle{slot, off, len, gen}. */
void orto_rt_ref(long *r, long count, long elemsize, void *init, long *out) {
  chk_reg(r);
  long s = r[0];
  long off = RT[s].used;
  long bytes = count * elemsize;
  if (off + bytes > RT[s].cap) abort();
  RT[s].used += bytes;
  if (init) { for (long i = 0; i < count; i++) memcpy(RT[s].buf + off + i*elemsize, init, (size_t)elemsize); }
  else      { memset(RT[s].buf + off, 0, (size_t)bytes); }
  out[0] = s; out[1] = off; out[2] = count; out[3] = r[1];
}

/* address of element i, gen+bounds checked (abort on failure). */
void *orto_rt_at(long *h, long i, long elemsize) {
  chk_h(h);
  if (i < 0 || i >= h[2]) abort();
  return RT[h[0]].buf + h[1] + i * elemsize;
}

/* address of element i, or NULL if stale/out-of-bounds (no abort) — for try_at. */
void *orto_rt_try(long *h, long i, long elemsize) {
  if (!RT[h[0]].live || RT[h[0]].gen != h[3]) return 0;
  if (i < 0 || i >= h[2]) return 0;
  return RT[h[0]].buf + h[1] + i * elemsize;
}

long orto_rt_len(long *h) { chk_h(h); return h[2]; }

/* base pointer of a handle's bytes (as_ptr / array_data) — gen checked. */
void *orto_rt_data(long *h) { chk_h(h); return RT[h[0]].buf + h[1]; }

void orto_rt_reset(long *r) {
  long s = r[0];
  RT[s].gen += 1; RT[s].used = 0; r[1] = RT[s].gen;   /* refresh the binding */
}

void orto_rt_drop(long *r) {
  long s = r[0];
  if (RT[s].live) { free(RT[s].buf); RT[s].buf = 0; RT[s].live = 0; RT[s].gen += 1; }
}

void orto_rt_slice(long *h, long lo, long hi, long elemsize, long *out) {
  chk_h(h);
  if (lo < 0 || hi > h[2] || lo > hi) abort();
  out[0] = h[0]; out[1] = h[1] + lo * elemsize; out[2] = hi - lo; out[3] = h[3];
}

/* wrap static/foreign bytes as a permanently-live Handle (string literals). */
void orto_rt_wrap(void *data, long len, long *out) {
  long s = rt_top++;
  if (s >= ORTO_MAXR) abort();
  RT[s].buf = (char *)data; RT[s].cap = len; RT[s].used = len;
  RT[s].gen += 1; RT[s].live = 1;
  out[0] = s; out[1] = 0; out[2] = len; out[3] = RT[s].gen;
}
