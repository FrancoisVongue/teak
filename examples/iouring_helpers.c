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

