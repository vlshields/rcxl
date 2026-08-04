#ifndef RCXL_XTHREAD_H
#define RCXL_XTHREAD_H

/* pthreads on every platform: Rtools ships winpthreads on Windows */
#include <pthread.h>

#ifdef _WIN32
#include <windows.h>
static int xthread_ncores(void)
{
    SYSTEM_INFO si;
    GetSystemInfo(&si);
    return si.dwNumberOfProcessors > 0 ? (int)si.dwNumberOfProcessors : 1;
}
#else
#include <unistd.h>
static int xthread_ncores(void)
{
    long n = sysconf(_SC_NPROCESSORS_ONLN);
    return n > 0 ? (int)n : 1;
}
#endif

#endif
