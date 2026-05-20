/* Thin synchronous wrappers over liburing for orto extern fn calls.
 *
 * Each operation submits exactly one SQE and waits for one CQE.
 * Return value is the io_uring `res` field (positive = success/bytes,
 * negative = -errno). This file is meant to be compiled and linked
 * with the orto-generated C output.
 *
 * Build:
 *   cc your_app.c iouring_helpers.c -luring -o your_app
 *
 * Why this layer exists:
 *   - liburing's struct io_uring is opaque + has a complex layout
 *     that doesn't translate cleanly through `extern fn` declarations.
 *   - orto has no inline C; this file is the smallest possible glue.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/socket.h>
#include <liburing.h>

/* Returns an opaque pointer-sized handle, or NULL on failure. The
 * handle is just a heap-allocated struct io_uring; the orto side
 * holds it inside a `linear struct Ring { handle: *byte }`. */
void *orto_ring_create(int entries) {
    struct io_uring *ring = malloc(sizeof(struct io_uring));
    if (!ring) return NULL;
    if (io_uring_queue_init((unsigned)entries, ring, 0) < 0) {
        free(ring);
        return NULL;
    }
    return ring;
}

int orto_ring_exit(void *handle) {
    if (!handle) return 0;
    struct io_uring *ring = handle;
    io_uring_queue_exit(ring);
    free(ring);
    return 0;
}

/* Submit one op, wait for its CQE, return its `res`. */
static int submit_and_wait(struct io_uring *ring) {
    int rc = io_uring_submit(ring);
    if (rc < 0) return rc;
    struct io_uring_cqe *cqe;
    rc = io_uring_wait_cqe(ring, &cqe);
    if (rc < 0) return rc;
    int res = cqe->res;
    io_uring_cqe_seen(ring, cqe);
    return res;
}

int orto_ring_read(void *handle, int fd, void *buf, int len, int offset) {
    struct io_uring *ring = handle;
    struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
    if (!sqe) return -ENOMEM;
    io_uring_prep_read(sqe, fd, buf, (unsigned)len, (unsigned long long)offset);
    return submit_and_wait(ring);
}

int orto_ring_write(void *handle, int fd, const void *buf, int len, int offset) {
    struct io_uring *ring = handle;
    struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
    if (!sqe) return -ENOMEM;
    io_uring_prep_write(sqe, fd, buf, (unsigned)len, (unsigned long long)offset);
    return submit_and_wait(ring);
}

int orto_ring_openat(void *handle, const char *path, int flags, int mode) {
    struct io_uring *ring = handle;
    struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
    if (!sqe) return -ENOMEM;
    io_uring_prep_openat(sqe, AT_FDCWD, path, flags, (mode_t)mode);
    return submit_and_wait(ring);
}

int orto_ring_close(void *handle, int fd) {
    struct io_uring *ring = handle;
    struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
    if (!sqe) return -ENOMEM;
    io_uring_prep_close(sqe, fd);
    return submit_and_wait(ring);
}

int orto_ring_fsync(void *handle, int fd) {
    struct io_uring *ring = handle;
    struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
    if (!sqe) return -ENOMEM;
    io_uring_prep_fsync(sqe, fd, 0);
    return submit_and_wait(ring);
}

int orto_ring_socket(void *handle, int domain, int type_, int protocol) {
    struct io_uring *ring = handle;
    struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
    if (!sqe) return -ENOMEM;
    io_uring_prep_socket(sqe, domain, type_, protocol, 0);
    return submit_and_wait(ring);
}

int orto_ring_connect(void *handle, int fd, const void *addr, int addrlen) {
    struct io_uring *ring = handle;
    struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
    if (!sqe) return -ENOMEM;
    io_uring_prep_connect(sqe, fd, (const struct sockaddr *)addr, (socklen_t)addrlen);
    return submit_and_wait(ring);
}

int orto_ring_accept(void *handle, int fd) {
    struct io_uring *ring = handle;
    struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
    if (!sqe) return -ENOMEM;
    io_uring_prep_accept(sqe, fd, NULL, NULL, 0);
    return submit_and_wait(ring);
}

int orto_ring_send(void *handle, int fd, const void *buf, int len, int flags) {
    struct io_uring *ring = handle;
    struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
    if (!sqe) return -ENOMEM;
    io_uring_prep_send(sqe, fd, buf, (size_t)len, flags);
    return submit_and_wait(ring);
}

int orto_ring_recv(void *handle, int fd, void *buf, int len, int flags) {
    struct io_uring *ring = handle;
    struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
    if (!sqe) return -ENOMEM;
    io_uring_prep_recv(sqe, fd, buf, (size_t)len, flags);
    return submit_and_wait(ring);
}

/* ---------- Stage 2: non-blocking submit + manual completion drain ----------
 *
 * Pattern from the caller (orto side):
 *
 *   uring_submit_read(r, fd, buf1, 0, 1)   // tag = 1
 *   uring_submit_read(r, fd, buf2, 0, 2)   // tag = 2
 *   uring_submit_read(r, fd, buf3, 0, 3)   // tag = 3
 *   uring_flush(r)                          // one syscall for all three
 *
 *   for i in 0..3 {
 *       let c = uring_wait_one(r);
 *       // c.id tells which one completed, c.res is its result
 *   }
 *
 * Tag (user_data) is just an int the kernel echoes back in the CQE.
 * Caller assigns it; common patterns: 0..N indices, struct pointer,
 * enum tag. */

int orto_ring_submit_read(void *handle, int fd, void *buf, int len,
                          int offset, int user_data) {
    struct io_uring *ring = handle;
    struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
    if (!sqe) return -ENOMEM;
    io_uring_prep_read(sqe, fd, buf, (unsigned)len, (unsigned long long)offset);
    io_uring_sqe_set_data(sqe, (void *)(long)user_data);
    return 0;
}

int orto_ring_submit_write(void *handle, int fd, const void *buf, int len,
                           int offset, int user_data) {
    struct io_uring *ring = handle;
    struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
    if (!sqe) return -ENOMEM;
    io_uring_prep_write(sqe, fd, buf, (unsigned)len, (unsigned long long)offset);
    io_uring_sqe_set_data(sqe, (void *)(long)user_data);
    return 0;
}

int orto_ring_submit_send(void *handle, int fd, const void *buf, int len,
                          int flags, int user_data) {
    struct io_uring *ring = handle;
    struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
    if (!sqe) return -ENOMEM;
    io_uring_prep_send(sqe, fd, buf, (size_t)len, flags);
    io_uring_sqe_set_data(sqe, (void *)(long)user_data);
    return 0;
}

int orto_ring_submit_recv(void *handle, int fd, void *buf, int len,
                          int flags, int user_data) {
    struct io_uring *ring = handle;
    struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
    if (!sqe) return -ENOMEM;
    io_uring_prep_recv(sqe, fd, buf, (size_t)len, flags);
    io_uring_sqe_set_data(sqe, (void *)(long)user_data);
    return 0;
}

int orto_ring_flush(void *handle) {
    return io_uring_submit((struct io_uring *)handle);
}

/* Block until one CQE arrives. Write user_data to *out_id; return
 * the CQE's res. */
int orto_ring_wait_one(void *handle, int *out_id) {
    struct io_uring *ring = handle;
    struct io_uring_cqe *cqe;
    int rc = io_uring_wait_cqe(ring, &cqe);
    if (rc < 0) { if (out_id) *out_id = 0; return rc; }
    if (out_id) *out_id = (int)(long)io_uring_cqe_get_data(cqe);
    int res = cqe->res;
    io_uring_cqe_seen(ring, cqe);
    return res;
}

/* Non-blocking peek: returns 0 if no CQE ready, 1 if one consumed
 * (out_res and out_id written), negative on error. */
int orto_ring_peek_one(void *handle, int *out_id, int *out_res) {
    struct io_uring *ring = handle;
    struct io_uring_cqe *cqe;
    int rc = io_uring_peek_cqe(ring, &cqe);
    if (rc == -EAGAIN) return 0;
    if (rc < 0) return rc;
    if (out_id)  *out_id  = (int)(long)io_uring_cqe_get_data(cqe);
    if (out_res) *out_res = cqe->res;
    io_uring_cqe_seen(ring, cqe);
    return 1;
}

