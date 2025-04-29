#include <Windows.h>    // Windows API headers
#include <stdio.h>      // Standard I/O functions

#define NUM_THREADS 64  // Number of threads to create

// Thread procedure that records precise start timestamps
DWORD __stdcall ThreadProc(void* start) {
    // Record high-resolution performance counter value when thread starts execution
    QueryPerformanceCounter((LARGE_INTEGER*)start);
    return 0;
}

int main(void) {
    // Array to store thread handles
    HANDLE threads[NUM_THREADS];
    // Array to store timestamps when CreateThread was called
    LARGE_INTEGER call_times[NUM_THREADS];
    // Array to store timestamps when threads actually started execution
    LARGE_INTEGER start_times[NUM_THREADS];

    // Create all threads and record creation timestamps
    for (int i = 0; i < NUM_THREADS; i++) {
        // Get precise timestamp before creating thread
        QueryPerformanceCounter(call_times + i);
        threads[i] = CreateThread(
            NULL,                   // Default security
            16 * 1024,              // Initial stack size 16KB
            ThreadProc,             // Thread entry point
            (void*)&start_times[i], // Parameter passed to thread
            0x00010000,             // STACK_SIZE_PARAM_IS_A_RESERVATION
            NULL                    // Don't store thread ID
        );
    }

    // Get frequency of performance counter for time calculations
    LARGE_INTEGER f;
    QueryPerformanceFrequency(&f);

    // Wait for all threads to complete execution
    WaitForMultipleObjects(NUM_THREADS, threads, TRUE, INFINITE);

    // Calculate and display timing results
    double total_time = 0.0;
    double max_time = 0.0;
    double min_time = 10000.0;
    for (int i = 0; i < NUM_THREADS; i++) {
        // Calculate time delta between thread creation and actual start in milliseconds
        double cost = 1000.0 * (start_times[i].QuadPart - call_times[i].QuadPart) / f.QuadPart;
        max_time = max_time > cost ? max_time : cost;
        min_time = min_time > cost ? cost : min_time;
        printf("%lf ms\n", cost);
        total_time += cost;
    }

    // Print summary statistics
    printf("total: %-8.5lf, average: %.5lf\n", total_time, total_time / NUM_THREADS);
    printf("max  : %-8.5lf, min    : %.5lf\n", max_time, min_time);

    return 0;
}
